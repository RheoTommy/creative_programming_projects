import { END, START, StateGraph, StateGraphArgs } from "@langchain/langgraph";
import { ChatOpenAI } from "@langchain/openai";
import { StructuredTool } from "@langchain/core/tools";
import { z } from "zod";
import {
    ChatPromptTemplate,
    MessagesPlaceholder,
} from "@langchain/core/prompts";
import { AIMessage, HumanMessage } from "@langchain/core/messages";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import { readLine } from "../utils.js";

const supervisorOptions = [
    "executor",
    "clarification",
    "rePlanner",
    "responder",
] as const;
type SupervisorOptions = (typeof supervisorOptions)[number];

export const initPlanAndExecuteGraph = (
    model: ChatOpenAI,
    tools: StructuredTool[],
) => {
    type State = {
        messages: (HumanMessage | AIMessage)[];
        plan?: string[];
        pastSteps: string[];
        nextAction?: SupervisorOptions;
    };

    const graphState: StateGraphArgs<State>["channels"] = {
        messages: {
            value: (
                x: (HumanMessage | AIMessage)[],
                y?: (HumanMessage | AIMessage)[],
            ) => x.concat(y ?? []),
            default: () => [],
        },
        plan: {
            value: (x?: string[], y?: string[]) => y ?? x,
            default: () => undefined,
        },
        pastSteps: {
            value: (x: string[], y: string[]) => x.concat(y),
            default: () => [],
        },
        nextAction: {
            value: (x?: SupervisorOptions, y?: SupervisorOptions) => y ?? x,
            default: () => undefined,
        },
    };

    const toolDescriptions = tools
        .map(
            (tool) =>
                `<Tool name=${tool.name}>\n\t${tool.description}\n</Tool>`,
        )
        .join("\n");

    const plannerNode = async (state: State) => {
        const prompt = ChatPromptTemplate.fromMessages([
            [
                "system",
                `You are a planner of an AI Agent. In order to complete the task given by the user, you need to come up with a plan. The plan should be a step-by-step guide to complete the task. Each step should be tool calls or a single action that an LLM can perform. In an execution step, it's possible to call multiple tools in parallel. The plan should be in sorted order. The final step should be the final answer. Make sure that each step has all the information needed - do not skip steps. The callable tools are as follows: \n${toolDescriptions}`,
            ],
            new MessagesPlaceholder("messages"),
        ]);
        const schema = z.object({
            plan: z
                .array(z.string().describe("different steps to follow"))
                .describe("The plan to follow"),
        });
        const chain = prompt.pipe(model.withStructuredOutput(schema));
        const res = await chain.invoke({ messages: state.messages });
        console.info(
            `Planner created a plan:\n${res.plan.map((x) => `\t - ${x}`).join("\n")}`,
        );
        return { plan: res.plan };
    };

    const executorNode = async (state: State) => {
        if (state.plan == undefined || state.plan.length == 0) {
            throw new Error("No more steps to execute");
        }
        const task = state.plan[0];
        if (task == undefined) {
            throw new Error("No more steps to execute");
        }

        const prompt = ChatPromptTemplate.fromMessages([
            new MessagesPlaceholder("messages"),
            [
                "ai",
                `You are an executor of an AI Agent. You need to execute the next step of the remaining plan. You can use tools. The past steps of the plan are:\n${JSON.stringify(state.pastSteps)}\nThe remaining steps of the plan are:\n${JSON.stringify(state.plan)}\n The The user input and the past steps are already given as messages.`,
            ],
        ]);
        const chain = prompt.pipe(model.bindTools(tools));
        const res = await chain.invoke({ messages: state.messages });

        console.info(`Executor executed step: ${task}`);
        return {
            messages: [res],
            plan: state.plan.slice(1),
            pastSteps: [task],
        };
    };

    const executorCondition = async (
        state: State,
    ): Promise<"tools" | "supervisor"> => {
        const messages = state.messages;
        const lastMessage = messages[messages.length - 1];
        if (!lastMessage) throw new Error("No messages in state");

        if ("tool_calls" in lastMessage && lastMessage.tool_calls.length > 0) {
            console.info(
                `Executor called tools: ${lastMessage.tool_calls.map((x) => JSON.stringify(x)).join(",\n")}`,
            );
            return "tools";
        } else {
            return "supervisor";
        }
    };

    const toolNode = new ToolNode<State>(tools);

    const supervisorNode = async (state: State) => {
        const prompt = ChatPromptTemplate.fromMessages([
            new MessagesPlaceholder("messages"),
            [
                "ai",
                `You are a supervisor of an AI Agent. The planner has come up with a plan. The past steps of the plan are:\n${JSON.stringify(state.pastSteps)}\nThe remaining steps of the plan are:\n${JSON.stringify(state.plan)}\nThe user input and the results of past steps are already given as messages. Please choose next action from the following options:
1. If the plan is successfully completed, then respond to the user with the final answer.
2. If the last step is successfully completed and the plan is not completed, then execute the next step.
3. If the last step didn't complete successfully: 
  a. retry the last step if it could be done better.
  b. re-plan the remaining steps.
4. If the execution is working but doesn't follow the plan, then fix the plan.
5. If there are some problems that can't be solved by yourself, then ask the user for help.
  - Note: Keep retrying or re-planning and failing is the worst option. The Agent execution could work better by clarifying the problem or asking the user for help.`,
            ],
        ]);
        const schema = z.object({
            option: z.enum(supervisorOptions).describe("Next action to take"),
            reason: z
                .string()
                .optional()
                .describe(
                    "The reason if the option is 'rePlanner' or 'clarification'",
                ),
        });
        const chain = prompt.pipe(model.withStructuredOutput(schema));
        const res = await chain.invoke({ messages: state.messages });

        const msg = new AIMessage(
            `Supervisor chose next action: ${res.option}` +
                (res.reason ? `\nwith reason: ${res.reason}` : ""),
        );
        console.info(
            `Supervisor chose next action: ${res.option}` +
                (res.reason ? `\nwith reason: ${res.reason}` : ""),
        );
        return { messages: [msg], nextAction: res.option };
    };

    const supervisorCondition = async (
        state: State,
    ): Promise<SupervisorOptions> => {
        if (state.nextAction == undefined) {
            throw new Error("No next action to take");
        }
        return state.nextAction;
    };

    const rePlannerNode = async (state: State) => {
        const prompt = ChatPromptTemplate.fromMessages([
            new MessagesPlaceholder("messages"),
            [
                "system",
                `You are a re-planner of an AI Agent. The planner has come up with a plan. The past steps of the plan are:\n${JSON.stringify(state.pastSteps)}\nThe remaining steps of the plan are:\n${JSON.stringify(state.plan)}\nThe user input and the results of past steps are already given as messages. You are asked to recreate the plan by the supervisor of the agent because of some reason (contained in messages.) Please re-plan the remaining steps.`,
            ],
        ]);
        const schema = z.object({
            plan: z
                .array(z.string().describe("different steps to follow"))
                .describe("The plan to follow"),
        });
        const chain = prompt.pipe(model.withStructuredOutput(schema));
        const res = await chain.invoke({ messages: state.messages });
        console.info(
            `Re-planner created a plan:\n${res.plan.map((x) => `\t - ${x}`).join("\n")}`,
        );
        return { plan: res.plan };
    };

    const clarificationNode = async (state: State) => {
        const prompt = ChatPromptTemplate.fromMessages([
            new MessagesPlaceholder("messages"),
            [
                "system",
                `You are a clarifier of an AI Agent. The planner has come up with a plan. The past steps of the plan are:\n${JSON.stringify(state.pastSteps)}\nThe remaining steps of the plan are:\n${JSON.stringify(state.plan)}\nThe user input and the results of past steps are already given as messages. Please ask the user for clarification to the problem. Your messages must be written in Japanese unless the user specifies otherwise. Your respond must be valid Markdown. Note: the user can't see the messages. They only know that the AI Agent is asking for clarification.`,
            ],
        ]);
        const res = await prompt
            .pipe(model)
            .invoke({ messages: state.messages });
        console.warn("Clarification:\n", res.content.toString());
        const input = await readLine("Respond: ");
        return {
            messages: [res, new HumanMessage(input)],
        };
    };

    const responderNode = async (state: State) => {
        const prompt = ChatPromptTemplate.fromMessages([
            new MessagesPlaceholder("messages"),
            [
                "system",
                `Given the messages below, please respond to the user. Your messages must be written in Japanese unless the user specifies otherwise. Your respond must be valid Markdown.`,
            ],
        ]);
        const res = await prompt
            .pipe(model)
            .invoke({ messages: state.messages });
        console.log(`Final Response:\n${res.content.toString()}`);
        return { messages: [res] };
    };

    return new StateGraph<State>({ channels: graphState })
        .addNode("planner", plannerNode)
        .addNode("executor", executorNode)
        .addNode("supervisor", supervisorNode)
        .addNode("rePlanner", rePlannerNode)
        .addNode("clarification", clarificationNode)
        .addNode("responder", responderNode)
        .addNode("tools", toolNode)
        .addEdge(START, "planner")
        .addEdge("planner", "executor")
        .addConditionalEdges("executor", executorCondition)
        .addEdge("tools", "supervisor")
        .addConditionalEdges("supervisor", supervisorCondition)
        .addEdge("rePlanner", "executor")
        .addEdge("clarification", "supervisor")
        .addEdge("responder", END);
};

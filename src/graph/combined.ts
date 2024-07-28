import { END, START, StateGraph, StateGraphArgs } from "@langchain/langgraph";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { z } from "zod";
import { gpt4o } from "../models.js";
import { initReActGraph } from "./react.js";
import { searchTool, tools, webContentWorker } from "../tools.js";
import { HumanMessage } from "@langchain/core/messages";
import { initPlanAndExecuteGraph } from "./planAndExecute.js";

type State = {
    input: string;
};
const graphState: StateGraphArgs<State>["channels"] = {
    input: {
        value: (a, b) => b,
        default: () => "",
    },
};

const modeCondition = async (state: State) => {
    const input = state.input;
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            "You are a mode selector of an AI Agent. Given the user's input below, select the mode: chatbot or plan_and_execute. Usually you will select chatbot mode, but if the user asks you to do something that could be long or complex execution, you should select plan_and_execute mode.",
        ],
        ["human", input],
    ]);
    const schema = z.object({
        mode: z.enum(["chatbot", "plan_and_execute"]),
    });
    const { mode } = await prompt
        .pipe(gpt4o.withStructuredOutput(schema))
        .invoke({ input });
    console.info(`Selected mode: ${mode}`);
    return mode;
};

export const combinedGraph = new StateGraph<State>({ channels: graphState })
    .addNode("react", (state) =>
        initReActGraph(gpt4o, tools)
            .compile()
            .invoke({ messages: [new HumanMessage(state.input)] }),
    )
    .addNode("plan_and_execute", (state) =>
        initPlanAndExecuteGraph(gpt4o, [searchTool, webContentWorker])
            .compile()
            .invoke({
                messages: [new HumanMessage(state.input)],
            }),
    )
    .addConditionalEdges(START, modeCondition, {
        chatbot: "react",
        plan_and_execute: "plan_and_execute",
    })
    .addEdge("react", END)
    .addEdge("plan_and_execute", END);

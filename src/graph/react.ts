// createReactAgent で代用可能だが、カスタマイズ性及び学習の観点から自作

import {
    AIMessage,
    HumanMessage,
    SystemMessage,
} from "@langchain/core/messages";
import { END, START, StateGraph, StateGraphArgs } from "@langchain/langgraph";
import { StructuredTool } from "@langchain/core/tools";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import { ChatOpenAI } from "@langchain/openai";
import { ChatGoogleGenerativeAI } from "@langchain/google-genai";
import {
    ChatPromptTemplate,
    MessagesPlaceholder,
} from "@langchain/core/prompts";

export const initReActGraph = (
    model: ChatOpenAI | ChatGoogleGenerativeAI,
    tools: StructuredTool[],
) => {
    type State = {
        messages: (HumanMessage | AIMessage | SystemMessage)[];
    };

    const graphState: StateGraphArgs<State>["channels"] = {
        messages: {
            value: (a, b) => a.concat(b),
            default: () => [],
        },
    };

    const toolNode = new ToolNode<State>(tools);
    const boundModel = model.bindTools(tools);

    const agentCondition = async (state: State) => {
        const messages = state.messages;
        const lastMessage = messages[messages.length - 1];
        if (!lastMessage) throw new Error("No messages in state");

        if ("tool_calls" in lastMessage && lastMessage.tool_calls.length > 0) {
            return "tools";
        } else {
            return END;
        }
    };

    const agentNode = async (state: State): Promise<State> => {
        const messages = state.messages;
        const prompt = ChatPromptTemplate.fromMessages([
            [
                "system",
                "You are an AI Agent. Your final task is to respond to the user. Given the recent messages below, do what you have to do as a next step in order to complete your final task. You can use the tools to help you. When you respond to the user, your messages must be written in Japanese unless the user specifies otherwise. Your respond must be valid Markdown.",
            ],
            new MessagesPlaceholder("messages"),
        ]);
        const chain = prompt.pipe(boundModel);
        const response = await chain.invoke({ messages });

        return {
            messages: [response],
        };
    };

    return new StateGraph<State>({ channels: graphState })
        .addNode("tools", toolNode)
        .addNode("agent", agentNode)
        .addEdge(START, "agent")
        .addConditionalEdges("agent", agentCondition)
        .addEdge("tools", "agent");
};

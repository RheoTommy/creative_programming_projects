// createReactAgent で代用可能だが、カスタマイズ性及び学習の観点から自作

import {
    AIMessage,
    HumanMessage,
    SystemMessage,
} from "@langchain/core/messages";
import { StateGraphArgs } from "@langchain/langgraph";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { StructuredTool } from "@langchain/core/tools";
import { ToolNode } from "@langchain/langgraph/prebuilt";

export const initReAct = (model: BaseChatModel, tools: StructuredTool[]) => {
    type State = {
        messages: (HumanMessage | AIMessage | SystemMessage)[];
    };

    const graphState: StateGraphArgs<State>["channels"] = {
        messages: {
            value: (a, b) => a.concat(b),
            default: () => [],
        },
    };

    const toolNode = new ToolNode<State>(tools)

    
};

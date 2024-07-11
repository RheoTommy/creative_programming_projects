import {HumanMessage} from "@langchain/core/messages";
import {END, MemorySaver, START, StateGraph, StateGraphArgs} from "@langchain/langgraph";
import {searchTool, webBrowser} from "../tools.js";
import {ToolNode} from "@langchain/langgraph/prebuilt";
import {gpt4o} from "../models.js";

const TEST_END = 'test_end'

const tools = [searchTool, webBrowser];
const toolNode = new ToolNode<AgentState>(tools);

const model = gpt4o.bindTools(tools);

type AgentState = {
    messages: HumanMessage[],
}

const graphState: StateGraphArgs<AgentState>["channels"] = {
    messages: {
        value: (x, y) => x.concat(y),
        default: () => [],
    }
}

const will_use_tool = (state: AgentState): "tools" | typeof END => {
    const messages = state.messages
    const lastMessage = messages[messages.length - 1]

    if (lastMessage?.additional_kwargs?.tool_calls) {
        return "tools"
    } else {
        return END
    }
}

const callModel = async (state: AgentState) => {
    const messages = state.messages;
    const [response] = await Promise.all([model.invoke(messages)]);

    return {
        messages: [response]
    }
}

const workflow = new StateGraph<AgentState>({channels: graphState})
    .addNode("agent", callModel)
    .addNode("tools", toolNode)
    .addEdge(START, "agent")
    .addConditionalEdges("agent", will_use_tool)
    .addEdge("tools", "agent")

const checkPointer = new MemorySaver()

const app = workflow.compile({checkpointer: checkPointer});

const finalState = await app.invoke(
    {messages: [new HumanMessage("今日の東京の天気は？")]},
    {configurable: {thread_id: '1'}}
)

console.log(finalState.messages)

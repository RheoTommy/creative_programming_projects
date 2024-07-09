import { END, START, StateGraph } from "@langchain/langgraph";
import { HumanMessage } from "@langchain/core/messages";
import { gpt4 } from "../models.js";
import { ToolNode } from "@langchain/langgraph/prebuilt";
import { ChatPromptTemplate, MessagesPlaceholder } from "@langchain/core/prompts";
import { searchTool } from "../tools.js";
const tools = [searchTool];
const toolNode = new ToolNode(tools);
const model = gpt4.bindTools(tools);
const graphState = {
    messages: {
        value: (x, y) => x.concat(y),
        default: () => [],
    }
};
// Define the function that determines whether to continue or not
const shouldContinue = (state) => {
    const { messages } = state;
    const lastMessage = messages[messages.length - 1];
    if (!lastMessage) {
        throw new Error("No messages in state");
    }
    // If there is no function call, then we finish
    if (!("tool_calls" in lastMessage.additional_kwargs) ||
        !lastMessage.additional_kwargs.tool_calls) {
        return "end";
    }
    // Otherwise if there is, we continue
    return "continue";
};
// Define the function that calls the model
const callModel = async (state, config) => {
    const { messages } = state;
    // You can use a prompt here to tweak model behavior.
    // You can also just pass messages to the model directly.
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", "You are a helpful assistant."],
        new MessagesPlaceholder("messages"),
    ]);
    const response = await prompt.pipe(model).invoke({ messages }, config);
    // We return a list, because this will get added to the existing list
    return { messages: [response] };
};
// Define a new graph
const workflow = new StateGraph({
    channels: graphState,
}) // Define the two nodes we will cycle between
    .addNode("agent", callModel)
    .addNode("tools", toolNode)
    // Set the entrypoint as `agent`
    // This means that this node is the first one called
    .addEdge(START, "agent")
    // We now add a conditional edge
    .addConditionalEdges(
// First, we define the start node. We use `agent`.
// This means these are the edges taken after the `agent` node is called.
"agent", 
// Next, we pass in the function that will determine which node is called next.
shouldContinue, 
// Finally we pass in a mapping.
// The keys are strings, and the values are other nodes.
// END is a special node marking that the graph should finish.
// What will happen is we will call `should_continue`, and then the output of that
// will be matched against the keys in this mapping.
// Based on which one it matches, that node will then be called.
{
    // If `tools`, then we call the tool node.
    continue: "tools",
    // Otherwise we finish.
    end: END,
})
    // We now add a normal edge from `tools` to `agent`.
    // This means that after `tools` is called, `agent` node is called next.
    .addEdge("tools", "agent");
// Finally, we compile it!
// This compiles it into a LangChain Runnable,
// meaning you can use it as you would any other runnable
const app = workflow.compile();
const inputs = { messages: [new HumanMessage("東京都知事選の立候補者の一覧を作成して")] };
const result = await app.invoke(inputs);
console.log(result);
//# sourceMappingURL=tmp.js.map
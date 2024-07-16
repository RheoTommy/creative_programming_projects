import {END, MemorySaver, START, StateGraph, StateGraphArgs} from "@langchain/langgraph";
import {AIMessage, HumanMessage} from "@langchain/core/messages";
import {gpt4o} from "../models.js";
import {ToolNode} from "@langchain/langgraph/prebuilt";
import {searchTool, webBrowser} from "../tools.js";
import {ChatPromptTemplate, MessagesPlaceholder} from "@langchain/core/prompts";
import {StringOutputParser} from "@langchain/core/output_parsers";
import {z} from "zod";
import {readLine} from "../utils.js";

const OUTPUT_PROMPT_SUFFIX = `回答は特段の指定がない限り日本語で生成し、適切なMarkdown記法を用いて行ってください。`

const nodeTypes = ["call_tool", "clarify", "respond"] as const;
type NodeTypes = typeof nodeTypes[number];

type AgentState = {
    messages: (HumanMessage | AIMessage)[],
    next?: NodeTypes,
}

const graphState: StateGraphArgs<AgentState>["channels"] = {
    messages: {
        value: (x, y?) => x.concat(y ?? []),
        default: () => [],
    },
    next: {
        value: (x?, y?) => y ?? x ?? undefined,
        default: () => undefined,
    }
}

const model = gpt4o;

const clarifyNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", `あなたはAI Agentです。今、Agentを実行するに当たりユーザーに確認を取るべき事項があり、実行が困難になっています。以下のmessagesを参照し、ユーザーに必要事項の確認を行ってください。\n${OUTPUT_PROMPT_SUFFIX}`],
        new MessagesPlaceholder("messages")
    ]);
    const messages = state.messages;
    const res = await prompt.pipe(model).pipe(new StringOutputParser()).invoke({messages});
    console.log(res);
    const inputs = await readLine("Answer: ")
    return {messages: [new HumanMessage(inputs)]};
}

const respondNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", `あなたはAI Agentです。今、Agentの実行が終了しました。以下のmessagesを参照し、ユーザーに返信を行ってください。\n${OUTPUT_PROMPT_SUFFIX}`],
        new MessagesPlaceholder("messages")
    ]);
    const messages = state.messages;
    const res = await prompt.pipe(model).pipe(new StringOutputParser()).invoke({messages});
    console.log(res);
    return {messages: [new AIMessage(res)]};
}

const tools = [searchTool, webBrowser]
const toolNode = new ToolNode<AgentState>(tools);
const toolModel = model.bindTools(tools);

const callToolNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", `あなたはAI Agentです。今、Agentの機能としてツールを実行する必要があります。以下のmessagesを参照し、ツールを呼び出してください。`],
        new MessagesPlaceholder("messages")
    ]);
    const messages = state.messages;
    const res = await prompt.pipe(toolModel).invoke({messages});
    return {messages: [res]};
}

const supervisorCondition = (state: AgentState): NodeTypes => {
    if (state.next == undefined) throw new Error("next is undefined");
    return state.next;
}

const supervisorNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", `あなたはAI Agentです。今、Agentの次の行動を決定する必要があります。以下のmessagesを参照し、次の行動を以下から選択してください。
1. respond: すでにAgentが行うべき処理は終了しているため、ユーザーへの返信を作成し処理を終える。
2. clarify: Agentの実行が自力ではこんなんだと思われる場合に、ユーザーに適切な質問を行い軌道修正を試みる。
  - ex1) ユーザーの質問・命令が不明瞭な場合：内容がはっきりわかるよう質問する。
  - ex2) Agentの実行が順調でない場合：Toolの使用時にエラーが発生し自己解決が難しい場合や、Agent処理の進行中に指示の曖昧さが判明したり、選択肢が複数生まれユーザーに決定を委ねるべきだったりするとき、適切にユーザーに質問をする。
3. call_tool: Agentの機能としてToolを呼び出す。
  - 使用可能なToolの説明：
{tool_descriptions}`],
        new MessagesPlaceholder("messages")
    ]);
    const tool_descriptions = tools.map(tool => `<Tool name=${tool.name}>${tool.description}</Tool>`).join("\n");
    const schema = z.object({
        kind: z.enum(nodeTypes),
    })
    const res = await prompt.pipe(model.withStructuredOutput(schema)).invoke({
        messages: state.messages,
        tool_descriptions
    });
    return {messages: [], next: res.kind};
}

const workflow = new StateGraph<AgentState>({channels: graphState})
    .addNode("supervisor", supervisorNode)
    .addNode("clarify", clarifyNode)
    .addNode("respond", respondNode)
    .addNode("call_tool", callToolNode)
    .addNode("tools", toolNode)
    .addEdge(START, "supervisor")
    .addConditionalEdges("supervisor", supervisorCondition)
    .addEdge("clarify", "supervisor")
    .addEdge("call_tool", "tools")
    .addEdge("tools", "supervisor")
    .addEdge("respond", END);


const checkpointer = new MemorySaver();

const app = workflow.compile({checkpointer});

const initState = {
    messages: new HumanMessage("fkdsagoi wfjadslk;a;d")
}

const finalState = await app.invoke(initState, {configurable: {thread_id: "1"}});
console.log(finalState.messages);
import { END, START, StateGraph, StateGraphArgs } from "@langchain/langgraph";
import { AIMessage, HumanMessage } from "@langchain/core/messages";
import { gpt4o } from "../models.js";
import { createReactAgent, ToolNode } from "@langchain/langgraph/prebuilt";
import { searchTool, webBrowser } from "../tools.js";
import {
    ChatPromptTemplate,
    MessagesPlaceholder,
} from "@langchain/core/prompts";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { z } from "zod";
import { readLine } from "../utils.js";

const OUTPUT_PROMPT_SUFFIX = `ユーザー向けに生成する回答は特段の指定がない限り日本語で生成し、適切なMarkdown記法を用いて行ってください。`;

const nodeTypes = [
    "__callTool__",
    "__clarify__",
    "__respond__",
    "__plan__",
] as const;
type NodeTypes = (typeof nodeTypes)[number];

type AgentState = {
    messages: (HumanMessage | AIMessage)[];
    next?: NodeTypes;
    reason?: string;
    plan?: string[];
};

const graphState: StateGraphArgs<AgentState>["channels"] = {
    messages: {
        value: (x, y?) => x.concat(y ?? []),
        default: () => [],
    },
    next: {
        value: (x?, y?) => y ?? x,
        default: () => undefined,
    },
    reason: {
        value: (x, y?) => y ?? x,
        default: () => "",
    },
    plan: {
        value: (x, y?) => y ?? x,
        default: () => [],
    },
};

const model = gpt4o;

const clarifyNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `あなたはAI Agentのclarify担当です。supervisorから以下の理由(reason)を受け今、Agentを実行するに当たりユーザーに確認を取るべき事項があり、実行が困難になっています。以下のmessagesを参照し、ユーザーに必要事項の確認を行ってください。\n${OUTPUT_PROMPT_SUFFIX}\n<reason>{reason}</reason>`,
        ],
        new MessagesPlaceholder("messages"),
    ]);
    const messages = state.messages;
    const reason = state.reason;
    const res = await prompt
        .pipe(model)
        .pipe(new StringOutputParser())
        .invoke({ messages, reason });
    console.log(`clarify: ${res}`);
    const inputs = await readLine("Answer: ");
    return { messages: [new HumanMessage(inputs)] };
};

const respondNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `あなたはAI Agentです。今、Agentの実行が終了しました。以下のmessagesを参照し、ユーザーに返信を行ってください。\n${OUTPUT_PROMPT_SUFFIX}`,
        ],
        new MessagesPlaceholder("messages"),
    ]);
    const messages = state.messages;
    const res = await prompt
        .pipe(model)
        .pipe(new StringOutputParser())
        .withConfig({ runName: "__respond__" })
        .invoke({ messages });
    console.log(`respond:\n${res}`);
    return { messages: [new AIMessage(res)] };
};

const tools = [searchTool, webBrowser];
const toolNode = new ToolNode<AgentState>(tools);
const toolModel = model.bindTools(tools);

const callToolNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `あなたはAI Agentです。今、Agentの機能としてツールを実行する必要があります。以下のmessagesを参照し、ツールを呼び出してください。
<reason>
    {reason}
</reason>`,
        ],
        new MessagesPlaceholder("messages"),
    ]);
    const messages = state.messages;
    const reason = state.reason;
    const res = await prompt
        .pipe(toolModel)
        .withConfig({ runName: "__callTool__" })
        .invoke({ messages, reason });
    return { messages: [res] };
};

const planNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `あなたはAI Agentです。今、Agentの実行が複数ステップによって行われるべき場合があります。以下のmessagesを参照し、長期タスクの実行手順を作成してください。
messagesにはユーザーの入力と、Agentの過去情報が含まれています。
すでに以前planを作成している場合、新たな情報を元に修正を行ってください。
また、Planの概要の説明と、次に行う内容の詳細をユーザー向けに説明してください。
${OUTPUT_PROMPT_SUFFIX}
<plan>
    {plan}
</plan>
<reason>
    {reason}
</reason>`,
        ],
        new MessagesPlaceholder("messages"),
    ]);
    const messages = state.messages;
    const plan = state.plan;
    const reason = state.reason;
    const schema = z.object({
        plan: z
            .array(z.string().describe("A step in the plan"))
            .describe("The plan"),
        descriptionOfPlan: z
            .string()
            .describe("The description of the whole plan"),
        descriptionOfNextStep: z
            .string()
            .describe("The description of the next step"),
    });
    const res = await prompt
        .pipe(model.withStructuredOutput(schema))
        .invoke({ messages, plan, reason });
    console.log(`plan: \n${res.descriptionOfPlan}`);
    console.log(`next step: \n${res.descriptionOfNextStep}`);
    return {
        messages: [
            new AIMessage(
                res.descriptionOfPlan + "\n" + res.descriptionOfNextStep,
            ),
        ],
        plan: res.plan,
    };
};

const supervisorCondition = (state: AgentState): NodeTypes => {
    if (state.next == undefined) throw new Error("next is undefined");
    return state.next;
};

const supervisorNode = async (state: AgentState): Promise<AgentState> => {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `あなたはAI Agentです。今、Agentの次の行動を決定する必要があります。以下のmessagesを参照し、次の行動を以下から選択してください。
1. __respond__: すでにAgentが行うべき処理は終了しているため、ユーザーへの返信を作成し処理を終える。
2. __clarify__: Agentの実行が自力ではこんなんだと思われる場合に、ユーザーに適切な質問を行い軌道修正を試みる。
  - ex1) ユーザーの質問・命令が不明瞭な場合：内容がはっきりわかるよう質問する。
  - ex2) Agentの実行が順調でない場合：Toolの使用時にエラーが発生し自己解決が難しい場合や、Agent処理の進行中に指示の曖昧さが判明したり、選択肢が複数生まれユーザーに決定を委ねるべきだったりするとき、適切にユーザーに質問をする。
3. __callTool__: Agentの機能としてToolを呼び出す。
  - 使用可能なToolの説明：
4. __plan__: ユーザーの命令の実行が複数ステップによって行われるべき場合に、長期タスクの実行手順を作成する。
  - 長期タスクとして実行する必要がある場合、はじめにPlanを作成する
  - タスク実行が順調でないとき、Planを修正する
{tool_descriptions}`,
        ],
        new MessagesPlaceholder("messages"),
    ]);
    const tool_descriptions = tools
        .map((tool) => `<Tool name=${tool.name}>${tool.description}</Tool>`)
        .join("\n");
    const schema = z.object({
        kind: z.enum(nodeTypes),
        reason: z.string(),
    });
    const res = await prompt
        .pipe(model.withStructuredOutput(schema))
        .withConfig({ runName: "__supervisor__" })
        .invoke({
            messages: state.messages,
            tool_descriptions,
        });
    console.info(`supervisor: ${res.reason}`);
    return {
        messages: [new AIMessage(res.reason)],
        next: res.kind,
        reason: res.reason,
    };
};

const workflow = new StateGraph<AgentState>({ channels: graphState })
    .addNode("__supervisor__", supervisorNode)
    .addNode("__clarify__", clarifyNode)
    .addNode("__respond__", respondNode)
    .addNode("__callTool__", callToolNode)
    .addNode("__tools__", toolNode)
    .addNode("__plan__", planNode)
    .addEdge(START, "__supervisor__")
    .addConditionalEdges("__supervisor__", supervisorCondition)
    .addEdge("__clarify__", "__supervisor__")
    .addEdge("__callTool__", "__tools__")
    .addEdge("__tools__", "__supervisor__")
    .addEdge("__plan__", "__supervisor__")
    .addEdge("__respond__", END);

// const checkpointer = new MemorySaver();
//
// const app = workflow.compile({ checkpointer });
//
// const initMessage = await readLine("Input: ");
//
// const initState = {
//     messages: new HumanMessage(initMessage),
// };
//
// const _finalState = await app.invoke(initState, {
//     configurable: { thread_id: "1" },
// });

export const agent = createReactAgent({ llm: gpt4o, tools });

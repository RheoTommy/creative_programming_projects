// test だが一般的な形式を取らない

import { describe, it } from "vitest";
import { agent, defaultAgent } from "../src/graph/agent.js";
import { HumanMessage } from "@langchain/core/messages";
import { Runnable } from "@langchain/core/runnables";

const agents = [
    {
        name: "myAgent",
        agent: agent,
    },
    {
        name: "defaultAgent",
        agent: defaultAgent,
    },
];
const withQuestions = (questions: string[]) =>
    agents.flatMap((ag) =>
        questions.map((q) => ({ q, name: ag.name, agent: ag.agent })),
    );

const runQuery = async ({ q, agent }: { q: string; agent: Runnable }) => {
    const res = await agent.invoke({
        messages: new HumanMessage(q),
    });
    console.info(res);
};

describe("Agent as a neutral LLM", () => {
    const questions = ["PythonでGCDを求める関数を非再帰で書いて。"];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);
});

describe("Agent with a single tool call", () => {
    const questions = [
        "LangChainJSの公式ドキュメントのリンクを調べて。", // Tavily Search
        "https://langchain-ai.github.io/langgraphjs/ を参照し、step-by-step breakdownを示して。", // WebBrowser
    ];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);

    // TODO: return incorrect 'breakdown' because WebBrowser doesn't return full content
});

describe("Agent with multiple tool calls", () => {
    const questions = [
        "Google GeminiをLangChainJSで呼び出す方法を調べ、教えて", // Tavily Search -> WebBrowser
    ];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);

    // TODO: MyAgent(ReAct) doesn't call WebBrowser so that it doesn't return specific content
});

describe("Agent with human interaction", () => {
    const questions = [
        "Google GeminiのAPIについての公式ドキュメントのリンクを取得し、その内容をMarkdown形式で書き直して。ただし、リンクを取得したらそれが正しいか人間に確認させて。", // Explicitly
        "https://js.lancain.com/v02/ocs/intons/plaos/openai この記事を日本語訳してMarkdown形式で書き直して。", // Implicitly
    ];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);

    // 高々数回の Tool Calling にて clarify node を用意せずとも、ReAct だけで十分では？
    // clarify node が必要なのは Long Task 特化のときのみでは？
    // TODO: myAgent doesn't ask for human clarification on the second query (keep retrying)
});

describe("Agent as a Long Task Executor", () => {
    const question = [
        "StackOverFlowの2024年度調査より、top likedなプログラミング言語を5つ調べ、各言語ごとに公式HPから最新のリリースノートを取得し一つのMarkdown記事にまとめよ。その後、各言語の実行速度の比較を行うため、適切な題材を調べ、すべての言語にて実装し、比較するためのシェルスクリプトを作成せよ。",
    ];
    const queries = withQuestions(question);

    it.each(queries)("Agent: $name, Question: $q", runQuery);

    // TODO: add more use cases
});

// test だが一般的な形式を取らない

import { describe, it } from "vitest";
import { HumanMessage } from "@langchain/core/messages";
import { Runnable } from "@langchain/core/runnables";
import { app } from "../src/graph/combined.js";

const sleep = (ms: number) => new Promise((res) => setTimeout(res, ms));

const agents = [
    // {
    //     name: "myAgent",
    //     agent: agent,
    // },
    // {
    //     name: "defaultAgent",
    //     agent: defaultAgent,
    // },
    {
        name: "combined",
        agent: app,
    },
];
const withQuestions = (questions: string[]) =>
    agents.flatMap((ag) =>
        questions.map((q) => ({ q, name: ag.name, agent: ag.agent })),
    );

const runQuery = async ({ q, agent }: { q: string; agent: Runnable }) => {
    const res = await agent.invoke({ input: q });
    console.info(res);
    await sleep(1000);
};

describe("Agent as a neutral LLM", () => {
    const questions = ["PythonでGCDを求める関数を非再帰で書いて。"];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);
});

describe("Agent with a single tool call", () => {
    const questions = [
        "LangChainJSの公式ドキュメントのリンクを調べて。", // Tavily Search
        "https://langchain-ai.github.io/langgraphjs/ を参照し、key featuresを示して。", // WebBrowser
    ];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);
});

describe("Agent with multiple tool calls", () => {
    const questions = [
        "@Mozilla/ReadabilityをLangChainJSで用いる方法を公式ドキュメントで調べて、簡潔にまとめて", // Tavily Search -> WebBrowser
    ];
    const queries = withQuestions(questions);

    it.each(queries)("Agent: $name, Question: $q", runQuery);
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
});

describe("Agent as a Long Task Executor", () => {
    const question = [
        "StackOverFlowの2024年度調査より、top likedなプログラミング言語を5つ調べ、各言語ごとに公式HPから最新のリリースノートを取得し一つのMarkdown記事にまとめよ。その後、各言語の実行速度の比較を行うため、適切な題材を調べ、すべての言語にて実装し、比較するためのシェルスクリプトを作成せよ。",
        "LangChainにて高度なAgentを作成するにはどうするべきか、公式ドキュメントを参照し、かんたんな日本語の解説を書いてください。また、その後適当なアプリケーション例を考え、実装例も示してください。",
    ];
    const queries = withQuestions(question);

    it.each(queries)("Agent: $name, Question: $q", runQuery);

    // TODO: add more use cases
});

// test だが一般的な形式を取らない

import { describe, it } from "vitest";
import { agent } from "../src/graph/agent.js";
import { HumanMessage } from "@langchain/core/messages";

describe("Agent as a neutral LLM", () => {
    const questions = ["PythonでGCDを求める関数を非再帰で書いて。"];

    // should respond quickly without any tool calls
    for (const q of questions) {
        it(`Question: ${q}`, async () => {
            const res = await agent.invoke({ messages: new HumanMessage(q) });
            console.info(res);
        });
    }
});

describe("Agent with a single tool call", () => {
    const questions = [
        "LangChainJSの公式ドキュメントのリンクを調べて。", // Tavily Search
        "https://langchain-ai.github.io/langgraphjs/ を参照し、step-by-step breakdownを示して。", // WebBrowser
    ];

    // should call a tool once and respond
    for (const q of questions) {
        it(`Question: ${q}`, async () => {
            const res = await agent.invoke({ messages: new HumanMessage(q) });
            console.info(res);
        });
    }

    // TODO: return incorrect 'breakdown' because WebBrowser doesn't return full content
});

describe("Agent with multiple tool calls", () => {
    const questions = [
        "LangChainJSの公式ドキュメントを参照し、Google GeminiをLangChainJSで呼び出す方法を教えて", // Tavily Search -> WebBrowser
    ];

    // should call multiple tools and respond
    for (const q of questions) {
        it(`Question: ${q}`, async () => {
            const res = await agent.invoke({ messages: new HumanMessage(q) });
            console.info(res);
        });
    }
});

describe("Agent with human interaction", () => {
    const questions = [
        "Google GeminiのAPIについての公式ドキュメントのリンクを取得し、その内容をMarkdown形式で書き直して。ただし、リンクを取得したらそれが正しいか人間に確認させて。", // Explicitly
        "https://js.langchain.com/v0.2/docs/intons/platforms/openai この記事を日本語訳してMarkdown形式で書き直して。", // Implicitly
    ];

    // should ask for clarification and respond
    for (const q of questions) {
        it(`Question: ${q}`, async () => {
            const res = await agent.invoke({ messages: new HumanMessage(q) });
            console.info(res);
        });
    }

    // 高々数回の Tool Calling にて clarify node を用意せずとも、ReAct だけで十分では？
    // clarify node が必要なのは Long Task 特化のときのみでは？
});

describe("Agent as a Long Task Executor", () => {
    const question = [
        "StackOverFlowの2023年度調査より、top likedなプログラミング言語を5つ調べ、各言語ごとに公式HPから最新のリリースノートを取得し一つのMarkdown記事にまとめよ。その後、各言語の実行速度の比較を行うため、適切な題材を調べ、すべての言語にて実装し、比較するためのシェルスクリプトを作成せよ。",
    ];

    for (const q of question) {
        it(
            `Question: ${q}`,
            async () => {
                const res = await agent.invoke({
                    messages: new HumanMessage(q),
                });
                console.info(res);
            },
            1000 * 60 * 10,
        );
    }

    // TODO: add more use cases
});

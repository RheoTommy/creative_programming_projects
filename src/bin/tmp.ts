import { HumanMessage } from "@langchain/core/messages";
import { initPlanAndExecuteGraph } from "../graph/planAndExecute.js";
import { gpt4o } from "../models.js";
import { searchTool, tools, webContentWorker } from "../tools.js";
import { agent } from "../graph/agent.js";
import { initReActGraph } from "../graph/react.js";
import { combinedGraph } from "../graph/combined.js";

// const r = await webContentWorker.invoke({
//     url: "https://js.langchain.com/v0.2/docs/tutorials/agents/",
//     order: "LangChainの公式ドキュメントから高度なAgentの作成方法に関する情報を抽出し、日本語で簡単な解説を書くために必要な情報を提供してください。具体的には、Agentの基本的な概念、作成手順、使用するツールや関数、実装例などを含めてください。",
// });

// const pae = initPlanAndExecuteGraph(gpt4o, [
//     searchTool,
//     webContentWorker,
// ]).compile();
//
// const rct = initReActGraph(gpt4o, [searchTool, webContentWorker]).compile();
//
// const res = await rct.invoke({
//     messages: [
//         new HumanMessage(
//             "LangChainにて高度なAgentを作成するにはどうするべきか、公式ドキュメントを参照し、かんたんな日本語の解説を書いてください。また、その後適当なアプリケーション例を考え、実装例も示してください。",
//             // "StackOverFlowの2024年度調査より、top likedなプログラミング言語を5つ調べ、各言語ごとに公式HPから最新のリリースノートを取得し一つのMarkdown記事にまとめよ。その後、各言語の実行速度の比較を行うため、適切な題材を調べ、すべての言語にて実装し、比較するためのシェルスクリプトを作成せよ。",
//         ),
//     ],
// });

// console.log(res);

const app = combinedGraph.compile();

await app.invoke({
    input: "LangChainにて高度なAgentを作成するにはどうするべきか、公式ドキュメントを参照し、かんたんな日本語の解説を書いてください。また、その後適当なアプリケーション例を考え、実装例も示してください。",
});

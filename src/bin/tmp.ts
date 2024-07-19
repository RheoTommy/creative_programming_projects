import { gpt4o } from "../models.js";
import { z } from "zod";
import { ChatPromptTemplate } from "@langchain/core/prompts";

const model = gpt4o;
const schema = z.object({
    kind: z.enum(["tools", "clarify", "plan", "END"]),
});
const prePrompt = `AI Agentの指示役として、ユーザーからの質問とこれまでの行動履歴を受け、次の行動を次の４つから選択してください
1. tools: ツールを用いて作業を行う
2. clarify: ユーザーへの確認なしで作業が困難だと思われるとき、ユーザーに確認するべきことを聞く
3. plan: ユーザーの命令の実行が複数ステップによって行われるべき場合に、長期タスクの実行手順を作成する
4. END: ユーザーに返信し、Agentの実行を終了する`;
const questions = [
    "こんにちは",
    "今日の東京の天気を教えて",
    "jfgowiidsjf;asjgsd",
    "今人気のAIライブラリを調べ、その上位5個の公式HPのリンクを取得して",
];

for (const question of questions) {
    const prompt = ChatPromptTemplate.fromMessages([
        ["system", prePrompt],
        ["human", "{question}"],
    ]);
    const chain = prompt.pipe(model.withStructuredOutput(schema));
    const response = await chain.invoke({ question });
    console.log(response);
}

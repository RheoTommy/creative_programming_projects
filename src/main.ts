import {ChatOpenAI} from "@langchain/openai";
import {ChatGoogleGenerativeAI} from "@langchain/google-genai";



const gpt = new ChatOpenAI({
    temperature: 0.6,
    model: "gpt-4-turbo-2024-04-09"
})

const gemini = new ChatGoogleGenerativeAI({
    temperature: 0.6,
    model: "gemini-1.5-pro-latest",
})

const question = "確率 1/n の事象を n 回繰り返し試行したとき、一度もその事象が起こらない確率を求め、定式化してください。また、これを n についてのグラフとして Python を用いて描画してください。"


// for await (const chunk of await gpt.stream(question)) {
//     process.stdout.write(chunk.content.toString());
// }
// console.log()
//
// console.log("--------------------")
//
for await (const chunk of await gemini.stream(question)) {
    process.stdout.write(chunk.content.toString());
}
console.log()
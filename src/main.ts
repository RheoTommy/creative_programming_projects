import {ChatOpenAI} from "@langchain/openai";
import {ChatGoogleGenerativeAI} from "@langchain/google-genai";
import {ChatPromptTemplate, PromptTemplate} from "@langchain/core/prompts";
import {StringOutputParser} from "@langchain/core/output_parsers";

const gpt = new ChatOpenAI({
    temperature: 0.6,
    model: "gpt-4-turbo-2024-04-09"
})

const gemini = new ChatGoogleGenerativeAI({
    temperature: 0.6,
    model: "gemini-pro",
})

const prompt = ChatPromptTemplate.fromMessages(["1/n の確率の事象を n 回繰り返したときに、少なくとも一度は事象が起こる確率を求めてください。また、これを n についてグラフ化するプログラムを R で作成してください。"]);

prompt.pipe(gemini).pipe()
const chains = [gpt, gemini].map(model => prompt.pipe(model).pipe(new StringOutputParser()));

for (const chain of chains) {
    const res = await chain.stream({});
    for await (const chunk of res) {
        process.stdout.write(chunk);
    }
    console.log();
}


import {ChatOpenAI} from "@langchain/openai";

const gpt = new ChatOpenAI({
    temperature: 0.6,
    streaming: true
})
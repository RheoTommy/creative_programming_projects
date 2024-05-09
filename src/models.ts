import {ChatOpenAI} from "@langchain/openai";
import {ChatGoogleGenerativeAI} from "@langchain/google-genai";

export const gpt4 = new ChatOpenAI({
    temperature: 0.6,
    model: "gpt-4-turbo-2024-04-09"
})

export const gemini = new ChatGoogleGenerativeAI({
    temperature: 0.6,
    model: "gemini-1.5-pro-latest",
})
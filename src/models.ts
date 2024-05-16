import {ChatOpenAI, OpenAIEmbeddings} from "@langchain/openai";
import {ChatGoogleGenerativeAI, GoogleGenerativeAIEmbeddings} from "@langchain/google-genai";

export const gpt4 = new ChatOpenAI({
    temperature: 0.6, model: "gpt-4o"
})

export const geminiPro = new ChatGoogleGenerativeAI({
    temperature: 0.6, model: "gemini-1.5-pro-latest",
})

export const geminiFlash = new ChatGoogleGenerativeAI({
    temperature: 0.6, model: "gemini-1.5-flash-latest"
})

export const gptEmbedding = new OpenAIEmbeddings();

export const geminiEmbedding = new GoogleGenerativeAIEmbeddings({
    model: "models/embedding-001",
});


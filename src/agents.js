import { searchTool, webBrowser } from "./tools.js";
import { MemoryVectorStore } from "langchain/vectorstores/memory";
import { gpt4, gptEmbedding } from "./models.js";
import { AutoGPT } from "langchain/experimental/autogpt";
import { AgentExecutor, createToolCallingAgent } from "langchain/agents";
import { ChatPromptTemplate } from "@langchain/core/prompts";
// AutoGPT doesn't support Gemini yet
const tools = [searchTool, webBrowser];
const vectorStore = new MemoryVectorStore(gptEmbedding);
export const autoGpt = AutoGPT.fromLLMAndTools(gpt4, tools, {
    memory: vectorStore.asRetriever(),
    aiName: "GPT",
    aiRole: "Assistant"
});
const prompt = ChatPromptTemplate.fromMessages([["system", "You are a helpful assistant"], ["placeholder", "{chat_history}"], ["human", "{input}"], ["placeholder", "{agent_scratchpad}"],]);
// AgentExecutor doesn't support Gemini yet cause Gemini doesn't have "bind_tools()" method.
// const geminiAgent = createToolCallingAgent({
//     llm: gemini,
//     tools,
//     prompt,
// });
// export const geminiExecutor = new AgentExecutor({agent: geminiAgent, tools})
const gpt4Agent = createToolCallingAgent({
    llm: gpt4, tools, prompt,
});
export const gpt4Executor = new AgentExecutor({ agent: gpt4Agent, tools });
//# sourceMappingURL=agents.js.map
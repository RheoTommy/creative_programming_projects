import {InMemoryFileStore} from "langchain/stores/file/in_memory";
import {searchTool} from "./tools.js";
import {ReadFileTool, WriteFileTool} from "langchain/tools";
import {MemoryVectorStore} from "langchain/vectorstores/memory";
import {gpt4, gpt_embedding} from "./models.js";
import {AutoGPT} from "langchain/experimental/autogpt";

// AutoGPT doesn't support Gemini yet

const store = new InMemoryFileStore();
const tools = [searchTool, new ReadFileTool({store}), new WriteFileTool({store})];
const vectorStore = new MemoryVectorStore(gpt_embedding)

export const autoGpt = AutoGPT.fromLLMAndTools(
    gpt4,
    tools,
    {memory: vectorStore.asRetriever(), aiName: "Gemini", aiRole: "Assistant"}
)

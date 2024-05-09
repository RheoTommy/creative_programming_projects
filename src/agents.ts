import {InMemoryFileStore} from "langchain/stores/file/in_memory";
import {searchTool} from "./tools.js";
import {ReadFileTool, WriteFileTool} from "langchain/tools";
import {MemoryVectorStore} from "langchain/vectorstores/memory";
import {OpenAIEmbeddings} from "@langchain/openai";
import {gemini_embedding} from "./models.js";

const store = new InMemoryFileStore();
const tools = [searchTool, new ReadFileTool({store}), new WriteFileTool({store})];
const vectorStore = new MemoryVectorStore(gemini_embedding)
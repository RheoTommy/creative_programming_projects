import {InMemoryFileStore} from "langchain/stores/file/in_memory";
import {searchTool} from "./tools.js";
import {ReadFileTool, WriteFileTool} from "langchain/tools";
import {MemoryVectorStore} from "langchain/dist/vectorstores/memory.js";

const store = new InMemoryFileStore();
const tools = [searchTool, new ReadFileTool({store}), new WriteFileTool({store})];
const vectorStore = new MemoryVectorStore(new )
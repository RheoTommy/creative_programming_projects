import {DuckDuckGoSearch} from "@langchain/community/tools/duckduckgo_search";
import {WebBrowser} from "langchain/tools/webbrowser";
import {geminiPro, geminiEmbedding} from "./models.js";

export const searchTool = new DuckDuckGoSearch();

export const webBrowser = new WebBrowser({model: geminiPro, embeddings: geminiEmbedding})
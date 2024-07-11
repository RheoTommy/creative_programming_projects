import {DuckDuckGoSearch} from "@langchain/community/tools/duckduckgo_search";
import {WebBrowser} from "langchain/tools/webbrowser";
import {geminiEmbedding, geminiPro} from "./models.js";

export const searchTool = new DuckDuckGoSearch();

export const webBrowser = new WebBrowser({model: geminiPro, embeddings: geminiEmbedding})

// export const res_for_bench_testing = 
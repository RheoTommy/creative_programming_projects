import { WebBrowser } from "langchain/tools/webbrowser";
import { geminiEmbedding, geminiFlash } from "./models.js";
import { TavilySearchResults } from "@langchain/community/tools/tavily_search";

export const searchTool = new TavilySearchResults();

export const webBrowser = new WebBrowser({
    model: geminiFlash,
    embeddings: geminiEmbedding,
});

export const tools = [searchTool, webBrowser];
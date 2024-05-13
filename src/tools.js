import { DuckDuckGoSearch } from "@langchain/community/tools/duckduckgo_search";
import { WebBrowser } from "langchain/tools/webbrowser";
import { gemini, gemini_embedding } from "./models.js";
export const searchTool = new DuckDuckGoSearch();
export const webBrowser = new WebBrowser({ model: gemini, embeddings: gemini_embedding });
//# sourceMappingURL=tools.js.map
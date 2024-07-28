import { WebBrowser } from "langchain/tools/webbrowser";
import { geminiEmbedding, geminiFlash } from "./models.js";
import { TavilySearchResults } from "@langchain/community/tools/tavily_search";
import { CheerioWebBaseLoader } from "@langchain/community/document_loaders/web/cheerio";
import { RecursiveCharacterTextSplitter } from "@langchain/textsplitters";
import { MozillaReadabilityTransformer } from "@langchain/community/document_transformers/mozilla_readability";
import { Document } from "@langchain/core/documents";
import { DynamicStructuredTool } from "@langchain/core/tools";
import { z } from "zod";
import { HtmlToTextTransformer } from "@langchain/community/document_transformers/html_to_text";
import { Readability } from "@mozilla/readability";
import { convert } from "html-to-text";

export const searchTool = new TavilySearchResults();

type WebContent = {
    content: string;
    source: string;
};
const getWebContent = async (url: string): Promise<WebContent> => {
    const res = await fetch(url, {
        headers: {
            "User-Agent":
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3",
        },
    });
    if (!res.ok) {
        throw new Error(`Failed to fetch: ${res.statusText}`);
    }
    const html = await res.text();
    const text = convert(html);

    const content = text;

    return { content, source: url };
};

export const webContentGetter = new DynamicStructuredTool({
    name: "WebContentGetter",
    description: "Useful for getting full content from a web page.",
    schema: z.object({ url: z.string().url().describe("URL") }),
    func: async ({ url }: { url: string }): Promise<string> => {
        try {
            const { content, source } = await getWebContent(url);
            return `<WebContent source="${source}">\n\t${content}\n</WebContent>`;
        } catch (e) {
            return JSON.stringify(e);
        }
    },
});

export const tools = [searchTool, webContentGetter];

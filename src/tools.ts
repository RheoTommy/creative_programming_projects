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

export const searchTool = new TavilySearchResults();

type WebContent = {
    content: string;
    source: string;
};
const getWebContent = async (url: string): Promise<WebContent> => {
    const loader = new CheerioWebBaseLoader(url);
    const docs = await loader.load();

    const splitter = RecursiveCharacterTextSplitter.fromLanguage("html");
    // const transformer = new MozillaReadabilityTransformer();
    const transformer = new HtmlToTextTransformer();

    const sequence = splitter.pipe(transformer);
    const newDocuments = (await sequence.invoke(docs)) as Document[]; // TODO: type check

    const source = newDocuments[0]?.metadata["source"] as string | undefined;
    const content = newDocuments.reduce(
        (acc, doc) => acc + doc.pageContent,
        "",
    );

    if (source == undefined) {
        throw new Error("Source not found");
    }

    return { content, source };
};

export const webContentGetter = new DynamicStructuredTool({
    name: "WebContentGetter",
    description: "Useful for getting full content from a web page.",
    schema: z.object({ url: z.string().url().describe("URL") }),
    func: async ({ url }: { url: string }): Promise<string> => {
        const { content, source } = await getWebContent(url);
        return `<WebContent source="${source}">\n\t${content}\n</WebContent>`;
    },
});

export const tools = [searchTool, webContentGetter];

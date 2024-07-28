import { TavilySearchResults } from "@langchain/community/tools/tavily_search";
import { DynamicStructuredTool } from "@langchain/core/tools";
import { z } from "zod";
import { convert } from "html-to-text";
import { gpt4oMini } from "./models.js";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { ChatPromptTemplate } from "@langchain/core/prompts";

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
    const content = convert(html);

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

export const webContentWorker = new DynamicStructuredTool({
    name: "WebContentWorker",
    description:
        "Useful when doing some work with the full content from a web page. This tool doesn't return the content. This tool let an LLM execute the task you ordered with the content. Your order must be written in detailed and include all the information needed.",
    schema: z.object({
        url: z.string().url().describe("URL"),
        order: z
            .string()
            .describe(
                "What you want an LLM to do with the content. MUST BE IN DETAILED AND INCLUDE ALL THE INFORMATION NEEDED. the longer 'order', the better respond.",
            ),
    }),
    func: async ({
        url,
        order,
    }: {
        url: string;
        order: string;
    }): Promise<string> => {
        try {
            const { content, source } = await getWebContent(url);
            const prompt = ChatPromptTemplate.fromMessages([
                [
                    "system",
                    `You are given a task to do with the content from a webpage. The content will be given to you as system message. And your task will be given as a human message.`,
                ],
                ["human", "{order}"],
                [
                    "system",
                    `<WebContent source="{source}">\n\t{content}\n</WebContent>`,
                ],
            ]);
            return await prompt
                .pipe(gpt4oMini)
                .pipe(new StringOutputParser())
                .invoke({ order, source, content });
        } catch (e) {
            console.error(e);
            return JSON.stringify(e);
        }
    },
});

export const tools = [searchTool, webContentGetter];

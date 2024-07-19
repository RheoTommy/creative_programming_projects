import {
    geminiEmbedding,
    geminiFlash,
    geminiPro,
    gpt4o,
    gpt4oMini,
    gptEmbedding,
} from "../src/models.js";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { searchTool, webBrowser } from "../src/tools.js";
import { describe, it } from "vitest";
import { streamToArray } from "../src/utils.js";

describe("Calling OpenAI Models", () => {
    it("Calling GPT-4o", async () => {
        const resGpt = await gpt4o.invoke("Hello, how are you?");
        console.log(resGpt);
    });

    it("Streaming GPT-4o", async () => {
        const resGpt = await gpt4o
            .pipe(new StringOutputParser())
            .stream("Hello, how are you?");
        console.log(await streamToArray(resGpt));
    });

    it("Calling GPT-4o-Mini", async () => {
        const resGptMini = await gpt4oMini.invoke("Hello, how are you?");
        console.log(resGptMini);
    });

    it("Streaming GPT-4o-Mini", async () => {
        const resGptMini = await gpt4oMini
            .pipe(new StringOutputParser())
            .stream("Hello, how are you?");
        console.log(await streamToArray(resGptMini));
    });

    it("Calling GPT-Embedding", async () => {
        const res = await gptEmbedding.embedQuery("Hello, how are you?");
        console.log(res);
    });
});

describe("Calling Gemini Models", () => {
    it("Calling Gemini-1.5-Pro-latest", async () => {
        const resGeminiPro = await geminiPro.invoke("Hello, how are you?");
        console.log(resGeminiPro);
    });

    it("Streaming Gemini-1.5-Pro-latest", async () => {
        const resGeminiPro = await geminiPro
            .pipe(new StringOutputParser())
            .stream("Hello, how are you?");
        console.log(await streamToArray(resGeminiPro));
    });

    it("Calling Gemini-1.5-Flash-latest", async () => {
        const resGeminiFlash = await geminiFlash.invoke("Hello, how are you?");
        console.log(resGeminiFlash);
    });

    it("Streaming Gemini-1.5-Flash-latest", async () => {
        const resGeminiFlash = await geminiFlash
            .pipe(new StringOutputParser())
            .stream("Hello, how are you?");
        console.log(await streamToArray(resGeminiFlash));
    });

    it("Calling Gemini-Embedding", async () => {
        const res2 = await geminiEmbedding.embedQuery("Hello, how are you?");
        console.log(res2);
    });
});

describe("Calling Tools", () => {
    it("Calling Tavily Search Tool", async () => {
        const res = await searchTool.invoke("TypeScript in FP");
        console.log(res);
    });

    it("Calling WebBrowse Tool", async () => {
        const url =
            "https://js.langchain.com/docs/integrations/tools/webbrowser";
        const res = await webBrowser.invoke(url);
        console.log(res);
    });
});

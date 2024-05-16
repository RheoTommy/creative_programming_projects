import {geminiEmbedding, geminiFlash, geminiPro, gpt4, gptEmbedding} from "./models.js";
import {executeComparison, streamToConsole} from "./utils.js";
import {StringOutputParser} from "@langchain/core/output_parsers";
import {searchTool, webBrowser} from "./tools.js";
import {loadTaskList} from "./loadTaskList.js";
import {autoGpt, gpt4Executor} from "./agents.js";
import {writeFileSync} from "node:fs";
import {readFileSync} from "fs";
import {resToMdFile} from "./outputViewer.js";

const callingModels = async () => {
    const resGpt = await gpt4.invoke("Hello, how are you?");
    console.log(resGpt);

    const resGeminiPro = await geminiPro.invoke("Hello, how are you?");
    console.log(resGeminiPro);

    const resGeminiFlash = await geminiFlash.invoke("Hello, how are you?")
    console.log(resGeminiFlash)
}

const streamingModels = async () => {
    const resGpt = await gpt4.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(resGpt);

    const resGeminiPro = await geminiPro.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(resGeminiPro);

    const resGeminiFlash = await geminiFlash.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(resGeminiFlash)
}

const callingEmbeddings = async () => {
    const res = await gptEmbedding.embedQuery("Hello, how are you?");
    console.log(res);

    const res2 = await geminiEmbedding.embedQuery("Hello, how are you?");
    console.log(res2);
}

const ddgSearch = async () => {
    const res = await searchTool.invoke("TypeScript in FP")
    console.log(res);
}

const webBrowse = async () => {
    const url = "https://js.langchain.com/docs/integrations/tools/webbrowser";
    const res = await webBrowser.invoke(url);
    console.log(res);
}

const printTaskList = () => {
    const tasks = loadTaskList();
    console.log(tasks);
}

const runAutoGpt = async () => {
    const res = await autoGpt.run(["Hello, how are you?"]);
    console.log(res)
}

const runComparison = async () => {
    // TODO: can't get logs from AutoGPT.run()
    const agents = [{
        agent: (question: string) => geminiPro.pipe(new StringOutputParser()).invoke(question),
        agentName: "Gemini-1.5-Pro"
    }, {
        agent: (question: string) => geminiFlash.pipe(new StringOutputParser()).invoke(question),
        agentName: "Gemini-1.5-Flash"
    }, {
        agent: (question: string) => gpt4.pipe(new StringOutputParser()).invoke(question), agentName: "GPT-4o"
    }, {
        agent: async (question: string) => (await gpt4Executor.invoke({input: question}))["output"] as string,
        agentName: "GPT-4o-Executor"
    }];

    const res = await executeComparison(agents, true);

    writeFileSync("../comparison.json", JSON.stringify(res, null, 2));
}

export const createMdFiles = async () => {
    const fs = readFileSync("../comparison.json", "utf-8");
    const data: {
        task: string; taskType: string; index: number; agentName: string; res: string;
    }[] = JSON.parse(fs);

    data.forEach(res => resToMdFile(res));
}

// await callingModels();
// await streamingModels();
// await callingEmbeddings();
// await ddgSearch();
// await webBrowse();
// printTaskList()
// await runAutoGpt()
// await runComparison()
// await createMdFiles()

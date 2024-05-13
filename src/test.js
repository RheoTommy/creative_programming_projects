import { gemini, gpt4, gpt_embedding } from "./models.js";
import { executeComparison, streamToConsole } from "./utils.js";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { searchTool } from "./tools.js";
import { loadTaskList } from "./loadTaskList.js";
import { autoGpt, gpt4Executor } from "./agents.js";
import { writeFileSync } from "node:fs";
import { readFileSync } from "fs";
import { resToMdFile } from "./outputViewer.js";
const callingModels = async () => {
    const res_gpt = await gpt4.invoke("Hello, how are you?");
    console.log(res_gpt);
    const res_gemini = await gpt4.invoke("Hello, how are you?");
    console.log(res_gemini);
};
const streamingModels = async () => {
    const res_gpt = await gpt4.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(res_gpt);
    const res_gemini = await gemini.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(res_gemini);
};
const callingEmbeddings = async () => {
    const res = await gpt_embedding.embedQuery("Hello, how are you?");
    console.log(res);
    const res2 = await gpt_embedding.embedQuery("Hello, how are you?");
    console.log(res2);
};
const ddgSearch = async () => {
    const res = await searchTool.invoke("TypeScript in FP");
    console.log(res);
};
const printTaskList = () => {
    const tasks = loadTaskList();
    console.log(tasks);
};
const runAutoGpt = async () => {
    const res = await autoGpt.run(["Hello, how are you?"]);
    console.log(res);
};
const runComparison = async () => {
    const agents = [
        {
            agent: (question) => gemini.pipe(new StringOutputParser()).invoke(question),
            agentName: "Gemini-1.5-Pro"
        },
        {
            agent: (question) => gpt4.pipe(new StringOutputParser()).invoke(question),
            agentName: "GPT-4-Turbo"
        },
        // {
        //     agent: async (question: string) => (await geminiExecutor.invoke({input: question}))["output"] as string,
        //     agentName: "Gemini-1.5-Pro-Executor"
        // },
        {
            agent: async (question) => (await gpt4Executor.invoke({ input: question }))["output"],
            agentName: "GPT-4-Turbo-Executor"
        },
        {
            agent: async (question) => await autoGpt.run([question]),
            agentName: "AutoGPT"
        }
    ];
    const res = await executeComparison(agents, true);
    writeFileSync("../comparison.json", JSON.stringify(res, null, 2));
};
export const createMdFiles = async () => {
    const fs = readFileSync("../comparison.json", "utf-8");
    const data = JSON.parse(fs);
    data.forEach(res => resToMdFile(res));
};
// await callingModels();
// await streamingModels();
// await callingEmbeddings();
// await ddgSearch();
// printTaskList()
// await runAutoGpt()
await runComparison();
await createMdFiles();
//# sourceMappingURL=test.js.map
import {gemini, gpt4, gpt_embedding} from "./models.js";
import {streamToConsole} from "./utils.js";
import {StringOutputParser} from "@langchain/core/output_parsers";
import {searchTool} from "./tools.js";
import {loadTaskList} from "./loadTaskList.js";
import {autoGpt} from "./agents.js";

const callingModels = async () => {
    const res_gpt = await gpt4.invoke("Hello, how are you?");
    console.log(res_gpt);

    const res_gemini = await gpt4.invoke("Hello, how are you?");
    console.log(res_gemini);
}

const streamingModels = async () => {
    const res_gpt = await gpt4.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(res_gpt);

    const res_gemini = await gemini.pipe(new StringOutputParser()).stream("Hello, how are you?");
    await streamToConsole(res_gemini);
}

const callingEmbeddings = async () => {
    const res = await gpt_embedding.embedQuery("Hello, how are you?");
    console.log(res);

    const res2 = await gpt_embedding.embedQuery("Hello, how are you?");
    console.log(res2);
}

const ddgSearch = async () => {
    const res = await searchTool.invoke("TypeScript in FP")
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

// await callingModels();
// await streamingModels();
// await callingEmbeddings();
// await ddgSearch();
// printTaskList()
runAutoGpt()

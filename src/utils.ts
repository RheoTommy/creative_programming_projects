import {IterableReadableStream} from "@langchain/core/utils/stream";
import {flattenTaskList, loadTaskList} from "./loadTaskList.js";
import * as fs from "node:fs";
import {writeFileSync} from "node:fs";
import {geminiFlash, geminiPro, gpt4} from "./models.js";
import {StringOutputParser} from "@langchain/core/output_parsers";
import {gpt4Executor} from "./agents.js";
import {writeResAsMd} from "./outputViewer.js";

export const streamToConsole = async <T>(stream: IterableReadableStream<T>) => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
}

export const executeTask = async (t: {
    task: string, taskType: string, index: number
}, a: {
    agent: (input: string) => Promise<string>, agentName: string
}) => {
    const res = await a.agent(t.task);
    return {
        ...t, ...a, res,
    }
}

export const executeComparison = async (agents: ({
    agent: (input: string) => Promise<string>, agentName: string,
})[], override: boolean): Promise<void> => {
    const tasks = flattenTaskList(loadTaskList())
    const runs = tasks.map(t => agents.map(a => ({t, a})))
        .flat()
        .map(async ({t, a}) => {
            const file = `./out/json/${t.taskType}-${t.index}-${a.agentName}.json`
            if (!override && fs.existsSync(file)) {
                return;
            }
            const res = await executeTask(t, a)
            writeFileSync(file, JSON.stringify(res, null, 2));
            writeResAsMd(res);
        });
    await Promise.all(runs);
}

export const runComparison = async (override: boolean = false): Promise<void> => {
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

    await executeComparison(agents, override);
}
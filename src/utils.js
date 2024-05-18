import { flattenTaskList, loadTaskList } from "./loadTaskList.js";
import { appendFileSync, writeFileSync } from "node:fs";
import { geminiFlash, geminiPro, gpt4 } from "./models.js";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { gpt4Executor } from "./agents.js";
export const streamToConsole = async (stream) => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
};
// TODO: define a func that executes a list of tasks with each agent and saves the results in a file
export const executeTask = async (t, a) => {
    const res = await a.agent(t.task);
    return {
        ...t, ...a, res,
    };
};
export const executeComparison = async (agents, autoSave = true) => {
    const tasks = flattenTaskList(loadTaskList());
    const runs = tasks.map(t => agents.map(a => ({ t, a })))
        .flat()
        .map(async ({ t, a }) => {
        const res = await executeTask(t, a);
        if (autoSave) {
            appendFileSync("./out.json", JSON.stringify(res) + ",\n");
        }
        return res;
    });
    return await Promise.all(runs);
};
const runComparison = async () => {
    // TODO: can't get logs from AutoGPT.run()
    const agents = [{
            agent: (question) => geminiPro.pipe(new StringOutputParser()).invoke(question),
            agentName: "Gemini-1.5-Pro"
        }, {
            agent: (question) => geminiFlash.pipe(new StringOutputParser()).invoke(question),
            agentName: "Gemini-1.5-Flash"
        }, {
            agent: (question) => gpt4.pipe(new StringOutputParser()).invoke(question), agentName: "GPT-4o"
        }, {
            agent: async (question) => (await gpt4Executor.invoke({ input: question }))["output"],
            agentName: "GPT-4o-Executor"
        }];
    const res = await executeComparison(agents, true);
    writeFileSync("./comparison.json", JSON.stringify(res, null, 2));
};
//# sourceMappingURL=utils.js.map
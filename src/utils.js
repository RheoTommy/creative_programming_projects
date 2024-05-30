import { flattenTaskList, loadTaskList } from "./loadTaskList.js";
import * as fs from "node:fs";
import { writeFileSync } from "node:fs";
import { geminiFlash, geminiPro, gpt4 } from "./models.js";
import { StringOutputParser } from "@langchain/core/output_parsers";
import { gpt4Executor } from "./agents.js";
import { writeResAsMd } from "./outputViewer.js";
export const streamToConsole = async (stream) => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
};
export const executeTask = async (t, a) => {
    const res = await a.agent(t.task);
    return {
        ...t, ...a, res,
    };
};
export const executeComparison = async (agents, override) => {
    const tasks = flattenTaskList(loadTaskList());
    const runs = tasks.map(t => agents.map(a => ({ t, a })))
        .flat()
        .map(async ({ t, a }) => {
        const file = `./out/json/${t.taskType}-${t.index}-${a.agentName}.json`;
        if (!override && fs.existsSync(file)) {
            return;
        }
        const res = await executeTask(t, a);
        writeFileSync(file, JSON.stringify(res, null, 2));
        writeResAsMd(res);
    });
    await Promise.all(runs);
};
export const runComparison = async (override = false) => {
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
    await executeComparison(agents, override);
};
//# sourceMappingURL=utils.js.map
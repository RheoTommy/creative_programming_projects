import { flattenTaskList, loadTaskList } from "./loadTaskList.js";
import { appendFileSync } from "node:fs";
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
            appendFileSync("../out.json", JSON.stringify(res) + ",\n");
        }
        return res;
    });
    return await Promise.all(runs);
};
//# sourceMappingURL=utils.js.map
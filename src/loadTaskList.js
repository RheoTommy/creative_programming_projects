import * as fs from "node:fs";
import yaml from "js-yaml";
export const loadTaskList = () => {
    const f = fs.readFileSync("../taskList.yaml", "utf-8");
    const data = yaml.load(f);
    return data;
};
export const flattenTaskList = (taskList) => {
    return [...taskList.simplest_questions.map((t, i) => ({
            task: t,
            taskType: "simplest_question",
            index: i
        })), ...taskList.single_task.map((t, i) => ({
            task: t,
            taskType: "single_task",
            index: i
        })), ...taskList.complex_task.map((t, i) => ({ task: t, taskType: "complex_task", index: i })),];
};
//# sourceMappingURL=loadTaskList.js.map
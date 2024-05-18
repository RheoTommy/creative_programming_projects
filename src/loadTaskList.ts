import * as fs from "node:fs";
import yaml from "js-yaml"

export type TaskData = {
    simplest_questions: string[]; single_task: string[]; complex_task: string[];
}

export const loadTaskList = () => {
    const f = fs.readFileSync("./taskList.yaml", "utf-8");
    const data: TaskData = yaml.load(f) as TaskData;
    return data;
}

export const flattenTaskList = (taskList: TaskData) => {
    return [...taskList.simplest_questions.map((t, i) => ({
        task: t,
        taskType: "simplest_question",
        index: i
    })), ...taskList.single_task.map((t, i) => ({
        task: t,
        taskType: "single_task",
        index: i
    })), ...taskList.complex_task.map((t, i) => ({task: t, taskType: "complex_task", index: i})),]
}
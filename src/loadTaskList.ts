import * as fs from "node:fs";
import yaml from "js-yaml"

export type TaskData = {
    simplest_questions: string[];
    single_task: string[];
    complex_task: string[];
}

export const loadTaskList = () => {
    const f = fs.readFileSync("../taskList.yaml", "utf-8");
    const data: TaskData = yaml.load(f) as TaskData;
    return data;
}
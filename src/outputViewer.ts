import {writeFileSync} from "node:fs";

const resToMdStr = (res: {
    task: string;
    taskType: string;
    index: number;
    agent: string;
    res: string;
}) => `
# ${res.taskType} - ${res.index}

- Agent: ${res.agent}

> Task: ${res.task}

# Response

${res.res}
`;

export const resToMdFile = (res: {
    task: string;
    taskType: string;
    index: number;
    agent: string;
    res: string;
}) => {
    const str = resToMdStr(res);
    writeFileSync(`../md/${res.taskType}-${res.index}-${res.agent}.md`, str);
}
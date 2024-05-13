import {writeFileSync} from "node:fs";

const resToMdStr = (res: {
    task: string; taskType: string; index: number; agentName: string; res: string;
}) => `
# ${res.taskType} - ${res.index}

- Agent: ${res.agentName}

> Task: ${res.task}

# Response

${res.res}
`;

export const resToMdFile = (res: {
    task: string; taskType: string; index: number; agentName: string; res: string;
}) => {
    const str = resToMdStr(res);
    writeFileSync(`../md/${res.taskType}-${res.index}-${res.agentName}.md`, str);
}
import {writeFileSync} from "node:fs";
import {readFileSync} from "fs";

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
    writeFileSync(`./out/md/${res.taskType}-${res.index}-${res.agentName}.md`, str);
}

export const createMdFiles = async () => {
    const fs = readFileSync("./out/comparison.json", "utf-8");
    const data: {
        task: string; taskType: string; index: number; agentName: string; res: string;
    }[] = JSON.parse(fs);

    data.forEach(res => resToMdFile(res));
}
import { writeFileSync } from "node:fs";
import { readFileSync } from "fs";
const resToMdStr = (res) => `
# ${res.taskType} - ${res.index}

- Agent: ${res.agentName}

> Task: ${res.task}

# Response

${res.res}
`;
export const resToMdFile = (res) => {
    const str = resToMdStr(res);
    writeFileSync(`./out/md/${res.taskType}-${res.index}-${res.agentName}.md`, str);
};
export const createMdFiles = async () => {
    const fs = readFileSync("./out/comparison.json", "utf-8");
    const data = JSON.parse(fs);
    data.forEach(res => resToMdFile(res));
};
//# sourceMappingURL=outputViewer.js.map
import { writeFileSync } from "node:fs";
const resToMdStr = (res) => `
# ${res.taskType} - ${res.index}

- Agent: ${res.agentName}

> Task: ${res.task}

# Response

${res.res}
`;
export const writeResAsMd = (res) => {
    const str = resToMdStr(res);
    writeFileSync(`./out/md/${res.taskType}-${res.index}-${res.agentName}.md`, str);
};
//# sourceMappingURL=outputViewer.js.map
import { flattenTaskList, loadTaskList } from "../loadTaskList.js";
import * as util from "node:util";
import * as child_process from "node:child_process";
import * as fs from "node:fs";
const taskList = flattenTaskList(loadTaskList());
const projectDir = process.cwd();
const exec = util.promisify(child_process.exec);
for (const { task, taskType, index } of taskList) {
    console.info(`Running task ${taskType}-${index}\ntask: ${task}`);
    const outJson = `./out/autoGpt/${taskType}-${index}.json`;
    const outMd = `./out/autoGpt/${taskType}-${index}.md`;
    if (fs.existsSync(outJson)) {
        console.info(`Task ${taskType}-${index} already done\n\n`);
        continue;
    }
    const arg = task.replace(/'/g, "'\\''");
    const cmd = `zsh -c "cd ${projectDir} && source ./loadEnv.zsh && node ./src/bin/runAutoGpt.js '${arg}'"`;
    const { stdout, stderr } = await exec(cmd);
    fs.appendFileSync(outJson, stdout);
    fs.appendFileSync(outMd, stderr);
    console.info(`Task ${taskType}-${index} done\n\n`);
}
//# sourceMappingURL=testAutoGpt.js.map
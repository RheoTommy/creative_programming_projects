import { autoGpt } from "../agents.js";
const task = process.argv[2];
if (!task) {
    console.error("Please provide a task to run");
    process.exit(1);
}
const res = await autoGpt.run([task]);
console.error(res);
//# sourceMappingURL=runAutoGpt.js.map
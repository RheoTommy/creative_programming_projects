import {flattenTaskList, loadTaskList} from "../loadTaskList.js";
import {autoGpt} from "../agents.js";

const taskList = flattenTaskList(loadTaskList());

const res = await autoGpt.run([taskList[0]!.task])

console.log(res)
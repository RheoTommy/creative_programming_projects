import {gpt4Executor} from "../agents.js";

const q = "RustとHaskellの公式ドキュメントを読み、思想の共通点、相違点をまとめてください。参考にしたURLを記載してください。";

const res = await gpt4Executor.invoke({input: q})

console.log(res);
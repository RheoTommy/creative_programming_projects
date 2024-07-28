import { readLine } from "../utils.js";
import { app } from "../graph/combined.js";

const q = await readLine("Enter a question: ");

await app.invoke({ input: q });

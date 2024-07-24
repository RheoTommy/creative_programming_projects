import { gpt4o } from "../models.js";
import { createReactAgent } from "@langchain/langgraph/prebuilt";
import { tools } from "../tools.js";

export const agent = createReactAgent({ llm: gpt4o, tools });

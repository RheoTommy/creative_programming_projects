import { gpt4o } from "../models.js";
import { tools } from "../tools.js";
import { MemorySaver } from "@langchain/langgraph";
import { initReActGraph } from "./react.js";
import { createReactAgent } from "@langchain/langgraph/prebuilt";

export const defaultAgent = createReactAgent({ llm: gpt4o, tools });

export const agent = initReActGraph(gpt4o, tools).compile();

import { agent, defaultAgent } from "../graph/agent.js";
import { HumanMessage } from "@langchain/core/messages";

const res = await defaultAgent.invoke({
    messages: new HumanMessage(
        "https://js.lancain.com/v02/ocs/intons/plaos/openai この記事を日本語訳してMarkdown形式で書き直して。",
    ),
});

console.log(res);

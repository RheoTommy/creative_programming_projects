import {describe, expect, it} from "vitest";
import {z} from "zod";
import {gpt4o} from "../src/models.js";
import {StringOutputParser} from "@langchain/core/output_parsers";
import {rewriteWithSchema} from "../src/utils.js";

describe("No Tool Tasks", () => {
    const schema = z.object({
        answer: z.string(),
    });
    const tasks = [
        {
            question: "日本の首都である都道府県名を答えよ",
            expected: "東京都",
        },
        {
            question: "RustでStackに保存される型に期待されるトレイト名を答えよ",
            expected: "Copy",
        }
    ];

    tasks.forEach(async ({question, expected}) => it(`question: ${question}`, async () => {
        const raw = await gpt4o.pipe(new StringOutputParser()).invoke(question);
        const res = await rewriteWithSchema(schema, raw);
        expect(res.answer).toBe(expected);
    }));
})
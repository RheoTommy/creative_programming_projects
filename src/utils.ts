import {IterableReadableStream} from "@langchain/core/utils/stream";
import {TypeOf, ZodObject, ZodRawShape} from "zod";
import {geminiFlash} from "./models.js";

export const streamToConsole = async <T>(stream: IterableReadableStream<T>): Promise<void> => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
}

export const streamToArray = async <T>(stream: IterableReadableStream<T>): Promise<T[]> => {
    const res = [];
    for await (const chunk of stream) {
        res.push(chunk);
    }
    return res;
}

export const rewriteWithSchema = async <T extends ZodRawShape>(
    schema: ZodObject<T>,
    rawOutput: string,
): Promise<TypeOf<ZodObject<T>>> => {
    const model = geminiFlash.withStructuredOutput(schema);
    const prompt = `Rewrite the following text with the schema:\n\n${rawOutput}\n\n`;
    const res = await model.invoke(prompt);
    return schema.parse(res);
}


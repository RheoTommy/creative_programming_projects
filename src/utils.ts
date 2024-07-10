import {IterableReadableStream} from "@langchain/core/utils/stream";

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
import {IterableReadableStream} from "@langchain/core/utils/stream";
import * as readline from "node:readline";

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

export const readLine = async (prompt?: string) : Promise<string> => {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    return new Promise((resolve) => {
        rl.question(prompt ?? "", (answer) => {
            resolve(answer);
            rl.close();
        });
    });
}
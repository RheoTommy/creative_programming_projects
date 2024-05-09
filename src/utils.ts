import {IterableReadableStream} from "@langchain/core/utils/stream";

export const streamToConsole = async <T>(stream: IterableReadableStream<T>) => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
}

// export const
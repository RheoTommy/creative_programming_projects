export const streamToConsole = async (stream) => {
    for await (const chunk of stream) {
        process.stdout.write(`${chunk}`);
    }
    console.log("");
};
export const streamToArray = async (stream) => {
    const res = [];
    for await (const chunk of stream) {
        res.push(chunk);
    }
    return res;
};
//# sourceMappingURL=utils.js.map
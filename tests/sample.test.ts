import {gpt4} from "../src/models.js";
import {expect, test} from "vitest";

test("Minimum test should work", () => {
    expect(1 + 1).toBe(2);
});
test("ChatGpt calling", async () => {
    console.log(await gpt4.invoke("こんにちは！"));
});
//# sourceMappingURL=sample.test.js.map
import { defineConfig } from "vitest/config";

export default defineConfig({
    test: {
        testTimeout: 60000, // タイムアウトを30秒に設定（デフォルトは5000ミリ秒）
    },
});

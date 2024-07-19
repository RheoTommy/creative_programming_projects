import { defineConfig } from "vitest/config";

export default defineConfig({
    test: {
        testTimeout: 30000, // タイムアウトを30秒に設定（デフォルトは5000ミリ秒）
    },
});

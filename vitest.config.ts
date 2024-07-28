import { defineConfig } from "vitest/config";

export default defineConfig({
    test: {
        testTimeout: 5*60*1000, // タイムアウトを30秒に設定（デフォルトは5000ミリ秒）
    },
});

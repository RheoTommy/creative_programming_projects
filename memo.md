# 各出力の分析

## AutoGPT

### simplest_question

- ツールを用いずにそのまま答えてほしい質問でも、複数回のツール使用をしてしまう。
- 提供していないツールを使用してしまう
  - AutoGPTのプロンプトの問題？
  - *必要以上にツールを使わないほうが良い*

### single_task

- 要求通りの動作
- 速度に難あり

### complex_task

- 5 top liked languages が見つけられずにずっと堂々巡り
- desired - admired の admired を認識してほしかった 

## 各種 LLM

### simplest_question

- LLM ごとに癖の違いがあるがおおむね期待通り

### single_task, complex_task

- ツール呼び出し不可能のためノーコメント

## GPT-4o Executor

### simplest_question

- [LangSmithでのデバッグ出力](https://smith.langchain.com/public/d4f61d6c-b5a7-4df5-96c1-e2da09e71d8b/r)を参照すると、不必要にツールを使わず答えられている。
- 出力はちょっとそっけない？

### single_task

- 出力は悪くない
- 50sほどで遅い
- [LangSmith](https://smith.langchain.com/public/7626c0d5-dadc-48ba-80aa-6fb67d892fd8/r)見るとちょっと無駄が多い？

### complex_task

- 60sほどで遅い
- AutoGPTと同じく堂々巡り


# 考察

- AutoGPTよりLangChainデフォルト実装のAgentの方が理想に近い動作をする
  - 必要に駆られたときに限り必要最低限のツール使用で実行
- complex_taskは文章が悪かったかも
  - 作成するAgentではこういった際に人の手で後から補足できたりするといい

# 目標

- LangChainデフォルト実装のAgentを次のように動作改善できるといい
  - インタラクティブなタスクの調整
  - ツール使用時の並列化（これもしかしてLangChainで実装された？）
- LangChain側の実装が日に日に進化しているため、プロジェクト目標設定時と違って新規性のないものになってしまうかも



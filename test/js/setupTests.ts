import prettier from "prettier";
import { spawnSync } from "child_process";

import type { Plugin } from "../../src/types";
import type { Code } from "./types";
import plugin from "../../src/plugin";

type Config = Partial<Plugin.Options>;

function normalize(code: Code) {
  const string = typeof code === "string" ? code : code.code;
  return string.replace(/\r?\n/g, "\n").trim();
}

function checkFormat(before: Code, after: Code, config: Config) {
  const originalText = typeof before === "string" ? before : before.code;
  const opts = {
    parser: typeof before === "string" ? "ruby" : before.parser,
    originalText,
    plugins: [plugin as any as string],
    ...config
  };

  if (opts.parser === "ruby") {
    const doc = (prettier as any).__debug.printToDoc(originalText, opts);
    const { stdout } = spawnSync("ruby", ["doc.rb"], { input: JSON.stringify(doc) });

    const expected = (prettier as any).__debug.printDocToString(doc, { parser: "ruby", plugins: ["."] }).formatted;
    const actual = stdout.toString();

    return {
      pass: expected === actual,
      message: () => `Expected:\n${expected}\nReceived:\n${actual}`
    };
  } else {
    const formatted = prettier.format(originalText, opts);
    const expected = normalize(after);
    const received = normalize(formatted);

    return {
      pass: received === expected,
      message: () => `Expected:\n${expected}\nReceived:\n${received}`
    };
  }
}

expect.extend({
  toChangeFormat(before: Code, after: Code, config: Config = {}) {
    return checkFormat(before, after, config);
  },
  toMatchFormat(before: Code, config: Config = {}) {
    return checkFormat(before, before, config);
  }
});

declare global {
  // eslint-disable-next-line @typescript-eslint/no-namespace
  namespace jest {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    interface Matchers<R> {
      toChangeFormat(after: Code, config?: Config): CustomMatcherResult;
      toMatchFormat(config?: Config): CustomMatcherResult;
    }
  }
}

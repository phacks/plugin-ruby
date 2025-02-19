import type { Plugin, Ruby } from "../../types";
import prettier from "../../prettier";

const { align, fill, group, hardline, indent, line } = prettier;

export const printCase: Plugin.Printer<Ruby.Case> = (path, opts, print) => {
  const parts: Plugin.Doc[] = ["case"];

  // You don't need to explicitly have something to test against in a case
  // statement (without it it effectively becomes an if/elsif chain).
  if (path.getValue().value) {
    parts.push(" ", path.call(print, "value"));
  }

  return [...parts, hardline, path.call(print, "cons"), hardline, "end"];
};

export const printWhen: Plugin.Printer<Ruby.When> = (path, opts, print) => {
  const node = path.getValue();

  // The `fill` builder command expects an array of docs alternating with
  // line breaks. This is so it can loop through and determine where to break.
  const preds = fill(
    (path.call(print, "args") as Plugin.Doc[]).reduce(
      (accum: Plugin.Doc[], pred, index) => {
        if (index === 0) {
          return [pred];
        }

        // Pull off the last element and make it concat with a comma so that
        // we can maintain alternating lines and docs.
        return [
          ...accum.slice(0, -1),
          [accum[accum.length - 1], ","],
          line,
          pred
        ];
      },
      [] as Plugin.Doc[]
    )
  );

  const stmts = path.call(print, "stmts") as Plugin.Doc[];
  const parts: Plugin.Doc[] = [["when ", align("when ".length, preds)]];

  // It's possible in a when to just have empty void statements, in which case
  // we would skip adding the body.
  if (!stmts.every((part) => !part)) {
    parts.push(indent([hardline, stmts]));
  }

  // This is the next clause on the case statement, either another `when` or
  // an `else` clause.
  if (node.cons) {
    parts.push(hardline, path.call(print, "cons"));
  }

  return group(parts);
};

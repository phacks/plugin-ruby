import type { Plugin, Ruby } from "../../types";
import prettier from "../../prettier";

const { group, indent, join, line, softline } = prettier;

export const printMAssign: Plugin.Printer<Ruby.Massign> = (
  path,
  opts,
  print
) => {
  const node = path.getValue();
  let valueDoc = path.call(print, "val");

  if (["mrhs", "mrhs_add_star", "mrhs_new_from_args"].includes(node.val.type)) {
    valueDoc = group(join([",", line], valueDoc));
  }

  const targetDoc: Plugin.Doc[] = [join([",", line], path.call(print, "tgt"))];
  if ((node.tgt as any).comma) {
    targetDoc.push(",");
  }

  return group([group(targetDoc), " =", indent([line, valueDoc])]);
};

export const printMLHS: Plugin.Printer<Ruby.Mlhs> = (path, opts, print) => {
  return path.map(print, "parts");
};

export const printMLHSAddPost: Plugin.Printer<Ruby.MlhsAddPost> = (
  path,
  opts,
  print
) => {
  return [
    ...(path.call(print, "star") as Plugin.Doc[]),
    ...(path.call(print, "mlhs") as Plugin.Doc[])
  ];
};

export const printMLHSAddStar: Plugin.Printer<Ruby.MlhsAddStar> = (
  path,
  opts,
  print
) => {
  const node = path.getValue();
  const parts: Plugin.Doc[] = ["*"];

  if (node.star) {
    parts.push(path.call(print, "star"));
  }

  return [...(path.call(print, "mlhs") as Plugin.Doc[]), parts];
};

export const printMLHSParen: Plugin.Printer<Ruby.MlhsParen> = (
  path,
  opts,
  print
) => {
  if (["massign", "mlhs_paren"].includes(path.getParentNode().type)) {
    // If we're nested in brackets as part of the left hand side of an
    // assignment, i.e., (a, b, c) = 1, 2, 3
    // ignore the current node and just go straight to the content
    return path.call(print, "cnts");
  }

  const node = path.getValue();
  const parts: Plugin.Doc[] = [
    softline,
    join([",", line], path.call(print, "cnts"))
  ];

  if ((node.cnts as any).comma) {
    parts.push(",");
  }

  return group(["(", indent(parts), [softline, ")"]]);
};

export const printMRHS: Plugin.Printer<Ruby.Mrhs> = (path, opts, print) => {
  return path.map(print, "parts");
};

export const printMRHSAddStar: Plugin.Printer<Ruby.MrhsAddStar> = (
  path,
  opts,
  print
) => {
  return [
    ...(path.call(print, "mrhs") as Plugin.Doc[]),
    ["*", path.call(print, "star")]
  ];
};

export const printMRHSNewFromArgs: Plugin.Printer<Ruby.MrhsNewFromArgs> = (
  path,
  opts,
  print
) => {
  return path.call(print, "args");
};

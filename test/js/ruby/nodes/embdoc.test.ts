import { ruby } from "../../utils";

describe("embdoc", () => {
  test("basic embdocs", () => {
    const content = ruby(`
      =begin
      this is

      some really

      long documentation
      that is contained
      in an embdoc
      =end
    `);

    expect(content).toMatchFormat();
  });

  test("within a class", () => {
    const content = ruby(`
      class Foo
      =begin
      this is an embdoc inside a class
      =end
      end
    `);

    expect(content).toMatchFormat();
  });

  test("within a nested class", () => {
    const content = ruby(`
      module Foo
        class Foo
      =begin
      this is an embdoc even more indented
      =end
        end
      end
    `);

    expect(content).toMatchFormat();
  });
});

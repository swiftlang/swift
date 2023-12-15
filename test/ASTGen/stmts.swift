
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

func test1(e b: Bool) {
  if b
  {
    print(
      "TRUE"
    )
  }
  else
  {
    print("FALSE")
  }
}

// RUN: echo 42 > %t.expected
// RUN: %target-run-simple-swift( \
// RUN:   -parse-as-library -module-name main \
// RUN:   -enable-experimental-feature Namespaces) > %t.legacy.out
// RUN: %diff -u %t.expected %t.legacy.out
// RUN: %target-run-simple-swift( \
// RUN:   -parse-as-library -module-name main \
// RUN:   -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen) > %t.astgen.out
// RUN: %diff -u %t.expected %t.astgen.out

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces
// UNSUPPORTED: asan

namespace BuildProof {
  static func answer() -> Int { 42 }
}

@main
struct Entry {
  static func main() {
    print(BuildProof.answer())
  }
}

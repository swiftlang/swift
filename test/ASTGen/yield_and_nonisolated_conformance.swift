// A `yield(...)` call in a coroutine is a `yield` statement, and a bare
// `nonisolated` on an inherited type is a type attribute. Both parsers accept
// these.

// RUN: %target-typecheck-verify-swift -enable-experimental-feature IsolatedConformances
// RUN: %target-typecheck-verify-swift -enable-experimental-feature IsolatedConformances \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_IsolatedConformances

struct S {
  var stored = 1
  var x: Int {
    _read { yield(stored) }
    _modify { yield (&stored) }
  }
}

struct MyType {}
extension MyType: nonisolated Equatable {
  nonisolated public static func == (a: MyType, b: MyType) -> Bool { true }
}

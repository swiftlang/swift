// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -typecheck -verify %s

// https://forums.swift.org/t/opaque-return-type-fails-when-using-variadic-generics/81120
// Opaque return types fail in extensions with pack expansion same-type constraints.

struct S1<each T> { }
struct S2<each T, U> { }

extension S2 where U == S1<repeat each T> {
  // These should compile - opaque return types in extensions with pack expansion
  // same-type constraints are valid.
  var property: some Any { () }
  func function() -> some Any { () }
}

// RUN: %target-typecheck-verify-swift -solver-scope-threshold=40000 -solver-enable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100000 -solver-disable-promote-supertypes

// At one point, this was the slowest expression (by number of scopes)
// in the source compatibility suite.

struct ID: CustomStringConvertible {
  var n: Int
  var description: String { "" }
}

struct Test {
  var id: ID
}

func slow(_ tests: LazyFilterSequence<some Sequence<Test>>) -> [String] {
  return Dictionary(
    grouping: tests.lazy.map(\.id),
    by: \.n
  ).values.lazy.map { x in (x, x.count > 1) }
    .flatMap { x, _ in x.lazy.map { x in } }
    .map(String.init(describing:))
    .sorted(by: <)
}

// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100000

struct ID: CustomStringConvertible {
  var n: Int
  var description: String { "" }
}

struct Test {
  var id: ID
}

func slow(_ tests: LazyFilterSequence<some Sequence<Test>>) -> [String] {
  // expected-error@+1 {{reasonable time}}
  return Dictionary(
    grouping: tests.lazy.map(\.id),
    by: \.n
  ).values.lazy.map { x in (x, x.count > 1) }
    .flatMap { x, _ in x.lazy.map { x in } }
    .map(String.init(describing:))
    .sorted(by: <)
}

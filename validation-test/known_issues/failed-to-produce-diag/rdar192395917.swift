// rdar://192395917
// RUN: %target-typecheck-verify-swift

struct S: Sequence {
  func makeIterator() -> IndexingIterator<[Any]> { fatalError() }
}

func f(x: [Int]) {
  _ = (x ?? S()).compactMap { $0 as? String } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

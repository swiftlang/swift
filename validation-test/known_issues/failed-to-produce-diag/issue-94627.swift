// rdar://193585221 (https://github.com/swiftlang/swift/issues/94627)
// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/pull/93539 added the unavailability score
// only for the last element of a multi-statement closure. If an unavailable
// overload is used in an earlier statement, the fallback error is still emitted.

struct Closed<Bound> {}

struct Half<Bound> {
  init(_ other: Closed<Bound>) {}
  @available(*, unavailable)
  init(_ other: Half<Bound>) {}
}

func run(_ body: () -> Void) {}

func test(source: Half<Int>) {
  run { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    let converted = Half(source)
    _ = converted
  }
}

// RUN: %target-swift-frontend -emit-ir -g %s

// Make sure we don't get the generic signature of the extension
// (<T, U where T: Equatable, U: Equatable>) mixed up with the
// generic signature of our function foo() (<T where T: Equatable>).

public struct G<T, U> {}

extension G where T: Equatable, U: Equatable {
  public struct Nested {}
}

public func foo<T>(_: G<T, T>.Nested) {
}

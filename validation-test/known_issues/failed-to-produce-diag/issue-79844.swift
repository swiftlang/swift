// rdar://146496171 (https://github.com/swiftlang/swift/issues/79844)
// RUN: %target-typecheck-verify-swift

protocol P<A> {
  associatedtype A
  func make() -> A
}

protocol Q: P<Int> {}

func source() -> any Q { fatalError() }

func takeValue(_ x: any P<Int>) {}

func test(value: any Q) {
  takeValue(source()) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  let x: any P<Int> = value // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  _ = x
}

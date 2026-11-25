// rdar://173289297 (https://github.com/swiftlang/swift/issues/88087)
// RUN: %target-typecheck-verify-swift

func makeTest<C: Collection>(arguments: @escaping @Sendable () async throws -> C, testFunction: @escaping @Sendable (C.Element) async throws -> Void) where C: Sendable, C.Element: Sendable {}
func makeTest<C: Collection, E1: Sendable, E2: Sendable>(arguments: @escaping @Sendable () async throws -> C, testFunction: @escaping @Sendable ((E1, E2)) async throws -> Void) where C: Sendable, C.Element == (E1, E2) {}

@Sendable func example(a: String?, b: Int?) async throws {}

func generator() {
  makeTest(arguments: { [(nil, 2), ("a", nil)] }, testFunction: example) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

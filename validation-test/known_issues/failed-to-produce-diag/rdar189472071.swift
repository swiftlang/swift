// rdar://189472071
// RUN: %target-typecheck-verify-swift

func XCTAssertEqual<T>(_: T, _: T) where T: Equatable {}

func f1(x: String) {
  XCTAssertEqual(x, x.count) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

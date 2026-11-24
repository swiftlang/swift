// rdar://189472071
// RUN: %target-typecheck-verify-swift

func XCTAssertEqual<T>(_: T, _: T) where T: Equatable {}

func f1(x: String) {
  XCTAssertEqual(x, x.count) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func f2() {
  _ = [].filter { _ in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    XCTAssertEqual(true, 2)
    return true
  }
}

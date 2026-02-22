// RUN: %target-typecheck-verify-swift

func test1<each T>(_ fn: (repeat each T, Int) -> Int) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func test2<each T>(_ fn: (repeat each T, Int?) -> Int) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func test3<each T>(_ fn: (repeat each T, String, Int) -> Void) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func validPackAtEnd<each T>(_ fn: (Int, repeat each T) -> Int) {
}

func validPackAfterPack<each T, each U>(_ fn: (repeat each T, repeat each U) -> Int) {
}

func validNoPack(_ fn: (Int, String) -> Int) {
}

func testMultiple<each T>(_ fn: (repeat each T, Int, String) -> Void) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func testNested<each T>(_ fn: (repeat each T, (Int) -> Int) -> Void) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func testGenericFunc<each T>(_ fn: (repeat each T, Int) -> Int) where repeat each T: Equatable {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
}

func issueExample<each T>(_ fn: (repeat each T, Int?) -> Int, _ arg: repeat each T) {
  // expected-error@-1 {{function type cannot have an unlabeled parameter following a variadic parameter pack; this signature is not supported}}
  fn(repeat each arg, 42)
}

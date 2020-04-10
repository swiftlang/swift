// RUN: %target-typecheck-verify-swift

func foo<T, U>(a: () -> T, b: () -> U) {}

foo { 42 }
b: { "" }

foo { 42 } b: { "" }

func when<T>(_ condition: @autoclosure () -> Bool,
             `then` trueBranch: () -> T,
             `else` falseBranch: () -> T) -> T {
  return condition() ? trueBranch() : falseBranch()
}

let _ = when (2 < 3) { 3 } else: { 4 }

struct S {
  static func foo(a: Int = 42, b: (inout Int) -> Void) -> S {
    return S()
  }

  static func foo(a: Int = 42, ab: () -> Void, b: (inout Int) -> Void) -> S {
    return S()
  }

  subscript(v v: () -> Int) -> Int {
    get { return v() }
  }

  subscript(u u: () -> Int, v v: () -> Int) -> Int {
    get { return u() + v() }
  }

  subscript(cond: Bool, v v: () -> Int) -> Int {
    get { return cond ? 0 : v() }
  }
  subscript(cond: Bool, u u: () -> Int, v v: () -> Int) -> Int {
    get { return cond ? u() : v() }
  }
}

let _: S = .foo {
  $0 = $0 + 1
}

let _: S = .foo {} b: { $0 = $0 + 1 }

func bar(_ s: S) {
  _ = s[] {
    42
  }

  _ = s[] {
    21
  } v: {
    42
  }

  _ = s[true] {
    42
  }

  _ = s[true] {
    21
  } v: {
    42
  }
}

func multiple_trailing_with_defaults(
  duration: Int,
  animations: (() -> Void)? = nil,
  completion: (() -> Void)? = nil) {}

multiple_trailing_with_defaults(duration: 42) {}

multiple_trailing_with_defaults(duration: 42) {} completion: {}

func test_multiple_trailing_syntax_without_labels() {
  func fn(f: () -> Void, g: () -> Void) {}

  fn {} g: {} // Ok

  fn {} _: {} // expected-error {{extra argument in call}}

  func multiple(_: () -> Void, _: () -> Void) {}

  multiple {} _: { }

  func mixed_args_1(a: () -> Void, _: () -> Void) {}
  func mixed_args_2(_: () -> Void, a: () -> Void, _: () -> Void) {} // expected-note {{'mixed_args_2(_:a:_:)' declared here}}

  mixed_args_1
    {}
    _: {}

  // FIXME: not a good diagnostic
  mixed_args_1
    {}  // expected-error {{extra argument in call}}
    a: {}

  mixed_args_2
    {}
    a: {}
    _: {}

  // FIXME: not a good diagnostic
  mixed_args_2
    {} // expected-error {{missing argument for parameter #1 in call}}
    _: {}

  // FIXME: not a good diagnostic
  mixed_args_2
    {} // expected-error {{extra argument in call}}
    _: {}
    _: {}
}

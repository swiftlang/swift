// RUN: %target-typecheck-verify-swift

func foo<T, U>(a: () -> T, b: () -> U) {}

foo {
  a: { 42 }
  b: { "" }
}

foo { a: { 42 } b: { "" } }

foo {
  a: { 42 }, // expected-error {{unexpected ',' separator}} {{12-13=}}
  b: { "" }
}

func when<T>(_ condition: @autoclosure () -> Bool,
             `then` trueBranch: () -> T,
             `else` falseBranch: () -> T) -> T {
  return condition() ? trueBranch() : falseBranch()
}

let _ = when (2 < 3) {
  then: { 3 }
  else: { 4 }
}

struct S {
  static func foo(a: Int = 42, b: (inout Int) -> Void) -> S {
    return S()
  }

  subscript(v v: () -> Int) -> Int {
    get { return v() }
  }

  subscript(cond: Bool, v v: () -> Int) -> Int {
    get { return cond ? 0 : v() }
  }
}

let _: S = .foo {
  b: { $0 = $0 + 1 }
}

func bar(_ s: S) {
  _ = s[] {
    v: { 42 }
  }

  _ = s[true] {
    v: { 42 }
  }
}

func multiple_trailing_with_defaults(
  duration: Int,
  animations: (() -> Void)? = nil,
  completion: (() -> Void)? = nil) {}

multiple_trailing_with_defaults(duration: 42) {
  animations: {}
}

multiple_trailing_with_defaults(duration: 42) {
  completion: {}
}

multiple_trailing_with_defaults(duration: 42) {
  animations: {}
  completion: {}
}

func test_multiple_trailing_syntax_without_labels() {
  func fn(f: () -> Void) {}

  fn {
    _: {} // Ok
  }

  fn {
    f: {} // Ok
  }

  func multiple(_: () -> Void, _: () -> Void) {}

  multiple {
    _: { }
    _: { }
  }

  func mixed_args_1(a: () -> Void, _: () -> Void) {}
  func mixed_args_2(_: () -> Void, a: () -> Void, _: () -> Void) {} // expected-note 2 {{'mixed_args_2(_:a:_:)' declared here}}

  mixed_args_1 {
    a: {}
    _: {}
  }

  mixed_args_1 {
    _: {}
    a: {} // expected-error {{argument 'a' must precede unnamed argument #1}}
  }

  mixed_args_2 {
    _: {}
    a: {}
    _: {}
  }

  mixed_args_2 {
    _: {} // expected-error {{missing argument for parameter 'a' in call}}
    _: {}
  }

  mixed_args_2 {
    a: {}
    _: {}
    _: {} // expected-error {{unnamed argument #3 must precede argument 'a'}}
  }

  mixed_args_2 {
    a: {} // expected-error {{missing argument for parameter #1 in call}}
    _: {}
  }
}

foo {
  a: { 42 }
  b: { 42 }

  _ = 1 + 2
  // expected-error@-1 {{expected an argument label followed by a closure literal}}
}

foo {
  a: { 42 }
  b: { 45 }
  c:
} // expected-error {{expected a closure literal}}

foo {
  a: { 42 }
  b: { 45 }
  c: 67 // expected-error {{expected a closure literal}}
  // expected-error@-1 {{extra argument 'c' in call}}
}

foo { // expected-note {{to match this opening '{'}}
  a: { 42 }
  b: { "" }

  func foo() {} // expected-error {{expected an argument label followed by a closure literal}}
  // expected-error@-1 {{expected '}' at the end of a trailing closures block}}
} // expected-error {{extraneous '}' at top level}}

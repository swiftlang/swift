// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClosureIsolation

// REQUIRES: swift_feature_ClosureIsolation

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

func multiple_trailing_with_defaults( // expected-note{{declared here}}
  duration: Int,
  animations: (() -> Void)? = nil,
  completion: (() -> Void)? = nil) {}

multiple_trailing_with_defaults(duration: 42) {} // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'completion' to suppress this warning}}

multiple_trailing_with_defaults(duration: 42) {} completion: {}

func test_multiple_trailing_syntax_without_labels() {
  func fn(f: () -> Void, g: () -> Void) {}

  fn {} g: {} // Ok

  fn {} _: {} // expected-error {{missing argument label 'g:' in call}} {{9-10=g}} {{none}}

  fn {} g: <#T##() -> Void#> // expected-error {{editor placeholder in source file}}

  func multiple(_: () -> Void, _: () -> Void) {}

  multiple {} _: { }

  func mixed_args_1(a: () -> Void, _: () -> Void) {}
  func mixed_args_2(_: () -> Void, a: () -> Void, _: () -> Void) {} // expected-note {{'mixed_args_2(_:a:_:)' declared here}}

  mixed_args_1 {} _: {}

  mixed_args_1 {} a: {}  // expected-error@:16 {{extraneous argument label 'a:' in call}} {{19-20=_}} {{none}}

  mixed_args_2 {} a: {} _: {}

  mixed_args_2 {} _: {} // expected-error@:18 {{missing argument for parameter 'a' in call}} {{18-18= a: <#() -> Void#>}} {{none}}

  mixed_args_2 {} _: {} _: {} // expected-error@:16 {{missing argument label 'a:' in call}} {{19-20=a}} {{none}}
}

func produce(fn: () -> Int?, default d: () -> Int) -> Int { // expected-note {{declared here}}
  return fn() ?? d()
}
// TODO: The diagnostics here are perhaps a little overboard.
_ = produce { 0 } default: { 1 } // expected-error {{missing argument for parameter 'default' in call}} expected-error {{consecutive statements}} expected-error {{'default' label can only appear inside a 'switch' statement}}
_ = produce { 2 } `default`: { 3 } // expected-error {{labeled block needs 'do'}} expected-warning {{integer literal is unused}}

func f() -> Int { 42 }

// This should be interpreted as a trailing closure, instead of being 
// interpreted as a computed property with undesired initial value.
struct TrickyTest {
    var x : Int = f () { // expected-error {{extra trailing closure passed in call}}
        3
    }
}

struct IsolationTest {
  func acceptClosureWithParameter(_: (Int) -> Int) {}

  func test() {
    acceptClosureWithParameter {
      nonisolated parameter in return parameter // expected-error {{the parameter list of a 'nonisolated' closure requires parentheses}} {{19-19=(}} {{29-29=)}}
    }
  }
}

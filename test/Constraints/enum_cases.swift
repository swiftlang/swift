// RUN: %target-typecheck-verify-swift -swift-version 4

// See test/Compatibility/enum_cases.swift for Swift 3 behavior

enum E {
  case foo(bar: String)
  case bar(_: String)
  case two(x: Int, y: Int)
  case tuple((x: Int, y: Int))
}

enum G_E<T> {
  case foo(bar: T)
  case bar(_: T)
  case two(x: T, y: T)
  case tuple((x: T, y: T))
}

let arr: [String] = []
let _ = arr.map(E.foo) // Ok
let _ = arr.map(E.bar) // Ok
let _ = arr.map(E.two) // expected-error {{cannot invoke 'map' with an argument list of type '(@escaping (Int, Int) -> E)'}}
// expected-note@-1{{expected an argument list of type '((Self.Element) throws -> T)'}}

let _ = arr.map(E.tuple) // expected-error {{cannot invoke 'map' with an argument list of type '(@escaping ((x: Int, y: Int)) -> E)'}}
// expected-note@-1{{expected an argument list of type '((Self.Element) throws -> T)'}}

let _ = arr.map(G_E<String>.foo) // Ok
let _ = arr.map(G_E<String>.bar) // Ok
let _ = arr.map(G_E<String>.two) // expected-error {{cannot invoke 'map' with an argument list of type '(@escaping (String, String) -> G_E<String>)'}}
// expected-note@-1{{expected an argument list of type '((Self.Element) throws -> T)'}}
let _ = arr.map(G_E<Int>.tuple) // expected-error {{cannot invoke 'map' with an argument list of type '(@escaping ((x: Int, y: Int)) -> G_E<Int>)'}}
// expected-note@-1{{expected an argument list of type '((Self.Element) throws -> T)'}}

let _ = E.foo("hello") // expected-error {{missing argument label 'bar:' in call}}
let _ = E.bar("hello") // Ok

let _ = G_E<String>.foo("hello") // expected-error {{missing argument label 'bar:' in call}}
let _ = G_E<String>.bar("hello") // Ok

// Passing enum case as an argument to generic function

func bar_1<T>(_: (T) -> E) {}
func bar_2<T>(_: (T) -> G_E<T>) {}
func bar_3<T, U>(_: (T) -> G_E<U>) {}

bar_1(E.foo) // Ok
bar_1(E.bar) // Ok
// SE-0110: We reverted to allowing this for the time being, but this
// test is valuable in case we end up disallowing it again in the
// future.
bar_1(E.two) // Ok since we backed off on this aspect of SE-0110 for the moment.
bar_1(E.tuple) // Ok - it's going to be ((x: Int, y: Int))

bar_2(G_E<String>.foo) // Ok
bar_2(G_E<Int>.bar) // Ok
bar_2(G_E<Int>.two) // expected-error {{cannot convert value of type '(Int, Int) -> G_E<Int>' to expected argument type '(Int) -> G_E<Int>'}}
bar_2(G_E<Int>.tuple) // expected-error {{cannot convert value of type '((x: Int, y: Int)) -> G_E<Int>' to expected argument type '(_) -> G_E<_>'}}
bar_3(G_E<Int>.tuple) // Ok

// Regular enum case assigned as a value

let foo: (String) -> E = E.foo // Ok
let _ = foo("hello") // Ok

let bar: (String) -> E = E.bar // Ok
let _ = bar("hello") // Ok

let two: (Int, Int) -> E = E.two // Ok
let _ = two(0, 42) // Ok

let tuple: ((x: Int, y: Int)) -> E = E.tuple // Ok
let _ = tuple((x: 0, y: 42)) // Ok

// Generic enum case assigned as a value

let g_foo: (String) -> G_E<String> = G_E<String>.foo // Ok
let _ = g_foo("hello") // Ok

let g_bar: (String) -> G_E<String> = G_E<String>.bar // Ok
let _ = g_bar("hello") // Ok

let g_two: (Int, Int) -> G_E<Int> = G_E<Int>.two // Ok
let _ = g_two(0, 42) // Ok

let g_tuple: ((x: Int, y: Int)) -> G_E<Int> = G_E<Int>.tuple // Ok
let _ = g_tuple((x: 0, y: 42)) // Ok


enum Foo {
  case a(x: Int)
  case b(y: Int)
}

func foo<T>(_: T, _: T) {}
foo(Foo.a, Foo.b) // Ok in Swift 4 because we strip labels from the arguments

// rdar://problem/32551313 - Useless SE-0110 diagnostic

enum E_32551313<L, R> {
  case Left(L)
  case Right(R)
}

struct Foo_32551313 {
  static func bar() -> E_32551313<(String, Foo_32551313?), (String, String)>? {
    return E_32551313.Left("", Foo_32551313()) // expected-error {{extra argument in call}}
  }
}

func rdar34583132() {
  enum E {
    case timeOut
  }

  struct S {
    func foo(_ x: Int) -> E { return .timeOut }
  }

  func bar(_ s: S) {
    guard s.foo(1 + 2) == .timeout else {
    // expected-error@-1 {{enum type 'E' has no case 'timeout'; did you mean 'timeOut'}}
      fatalError()
    }
  }
}

func rdar_49159472() {
  struct A {}
  struct B {}
  struct C {}

  enum E {
  case foo(a: A, b: B?)

    var foo: C? {
      return nil
    }
  }

  class Test {
    var e: E

    init(_ e: E) {
      self.e = e
    }

    func bar() {
      e = .foo(a: A(), b: nil)   // Ok
      e = E.foo(a: A(), b: nil)  // Ok
      baz(e: .foo(a: A(), b: nil))  // Ok
      baz(e: E.foo(a: A(), b: nil)) // Ok
    }

    func baz(e: E) {}
  }
}

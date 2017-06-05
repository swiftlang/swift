// RUN: %target-typecheck-verify-swift -swift-version 3

// Tests for enum case behavior in Swift 3

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
let _ = arr.map(E.two) // expected-error {{cannot convert value of type '(Int, Int) -> E' to expected argument type '(String) -> _'}}
let _ = arr.map(E.tuple) // expected-error {{cannot convert value of type '((x: Int, y: Int)) -> E' to expected argument type '(String) -> _'}}

let _ = arr.map(G_E<String>.foo) // Ok
let _ = arr.map(G_E<String>.bar) // Ok
let _ = arr.map(G_E<String>.two) // expected-error {{cannot convert value of type '(String, String) -> G_E<String>' to expected argument type '(String) -> _'}}
let _ = arr.map(G_E<Int>.tuple) // expected-error {{cannot convert value of type '((x: Int, y: Int)) -> G_E<Int>' to expected argument type '(String) -> _'}}

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
bar_1(E.two) // Ok in Swift 3
bar_1(E.tuple) // Ok - it's going to be ((x: Int, y: Int))

bar_2(G_E<String>.foo) // Ok
bar_2(G_E<Int>.bar) // Ok
bar_2(G_E<Int>.two) // expected-error {{cannot convert value of type '(Int, Int) -> G_E<Int>' to expected argument type '(_) -> G_E<_>'}}
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

// Following example doesn't work in Swift 3 mode because Foo.a and Foo.b have different argument types
// since the labels are not striped from them.

enum Foo {
  case a(x: Int)
  case b(y: Int)
}

func foo<T>(_: T, _: T) {}
foo(Foo.a, Foo.b) // expected-error {{cannot invoke 'foo' with an argument list of type '((Int) -> Foo, (Int) -> Foo)}}
// expected-note@-1 {{expected an argument list of type '(T, T)'}}

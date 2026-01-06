// RUN: %target-typecheck-verify-swift -swift-version 6

@ MainActor  // expected-error {{extraneous whitespace between '@' and attribute name}}
class Foo {
  func funcWithEscapingClosure(_ x: @ escaping () -> Int) {} // expected-error {{extraneous whitespace between '@' and attribute name}}
}

@available (*, deprecated) // expected-error {{extraneous whitespace between attribute name and '('}}
func deprecated() {}

@propertyWrapper
struct MyPropertyWrapper {
  var wrappedValue: Int = 1

  init(param: Int) {}
}

struct PropertyWrapperTest {
  @MyPropertyWrapper (param: 2)  // expected-error {{extraneous whitespace between attribute name and '('}}
  var x: Int

  @MyPropertyWrapper
  (param: 2) // expected-error {{expected 'var' keyword in property declaration}} expected-error {{property declaration does not bind any variables}} expected-error {{expected pattern}}
  var y: Int
}

let closure1 = { @MainActor (a, b) in 
// expected-error@-1 {{cannot infer type of closure parameter 'a' without a type annotation}}
// expected-error@-2 {{cannot infer type of closure parameter 'b' without a type annotation}}
}

let closure2 = { @MainActor
  (a: Int, b: Int) in
  let _: Int = a
}

// expected-error@+1 {{extraneous whitespace between '@' and attribute name}}
@ 
MainActor
func mainActorFunc() {}


@inline // expected-error {{expected '(' in 'inline' attribute}}
(never) func neverInline() {} // expected-error {{expected declaration}}

@objc
(whatever) func whateverObjC() {} // expected-error {{expected declaration}}

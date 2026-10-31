// RUN: %target-typecheck-verify-swift -swift-version 6

@ MainActor  // expected-cparser-error {{extraneous whitespace between '@' and attribute name}} expected-astgen-error {{extraneous whitespace after '@' is not permitted}}
class Foo {
  func funcWithEscapingClosure(_ x: @ escaping () -> Int) {} // expected-cparser-error {{extraneous whitespace between '@' and attribute name}}
}

@available (*, deprecated) // expected-cparser-error {{extraneous whitespace between attribute name and '('}} expected-astgen-error {{extraneous whitespace before '(' is not permitted}}
func deprecated() {}

@propertyWrapper
struct MyPropertyWrapper {
  var wrappedValue: Int = 1

  init(param: Int) {}
}

struct PropertyWrapperTest {
  @MyPropertyWrapper (param: 2)  // expected-cparser-error {{extraneous whitespace between attribute name and '('}} expected-astgen-error {{extraneous whitespace before '(' is not permitted}}
  var x: Int

  @MyPropertyWrapper
  (param: 2) // expected-cparser-error {{expected 'var' keyword in property declaration}} expected-cparser-error {{property declaration does not bind any variables}} expected-cparser-error {{expected pattern}} expected-astgen-error {{unexpected code '(param: 2)' in variable}}
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

// expected-cparser-error@+1 {{extraneous whitespace between '@' and attribute name}} expected-astgen-error@+1 {{extraneous whitespace after '@' is not permitted}}
@ 
MainActor
func mainActorFunc() {}


@inline // expected-error {{expected '(' in 'inline' attribute}}
(never) func neverInline() {} // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code '(never)' in function}}

@objc
(whatever) func whateverObjC() {} // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code '(whatever)' in function}}

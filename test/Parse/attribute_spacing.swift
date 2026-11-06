// RUN: %target-typecheck-verify-swift

@ MainActor  // expected-cparser-warning {{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}} expected-astgen-warning {{extraneous whitespace after '@' is not permitted; this is an error in Swift 6}}
class Foo {
  func funcWithEscapingClosure(_ x: @ escaping () -> Int) {} // expected-cparser-warning {{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}}
}

@available (*, deprecated) // expected-cparser-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}} expected-astgen-warning {{extraneous whitespace before '(' is not permitted; this is an error in Swift 6}}
func deprecated() {}

@propertyWrapper
struct MyPropertyWrapper {
  var wrappedValue: Int = 1

  init(param: Int) {}
}

struct PropertyWrapperTest {
  @MyPropertyWrapper (param: 2)  // expected-cparser-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}} expected-astgen-warning {{extraneous whitespace before '(' is not permitted; this is an error in Swift 6}}
  var x: Int

  @MyPropertyWrapper
  (param: 2) // expected-cparser-error {{expected 'var' keyword in property declaration}} expected-cparser-error {{property declaration does not bind any variables}} expected-cparser-error {{expected pattern}} expected-astgen-error {{unexpected code '(param: 2)' in variable}}
  var y: Int
}

let closure1 = { @MainActor (a, b) in // expected-cparser-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}} expected-astgen-warning {{extraneous whitespace before '(' is not permitted; this is an error in Swift 6}}
}

let closure2 = { @MainActor
  (a: Int, b: Int) in
}

// expected-cparser-warning@+1{{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}} expected-astgen-warning@+1 {{extraneous whitespace after '@' is not permitted; this is an error in Swift 6}}
@ 
MainActor
func mainActorFunc() {}


@inline // expected-error {{expected '(' in 'inline' attribute}}
(never) func neverInline() {} // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code '(never)' in function}}

@objc
(whatever) func whateverObjC() {} // expected-cparser-error {{expected declaration}} expected-astgen-error {{unexpected code '(whatever)' in function}}

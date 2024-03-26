// RUN: %target-typecheck-verify-swift

@ MainActor  // expected-warning {{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}}
class Foo {
  func funcWithEscapingClosure(_ x: @ escaping () -> Int) {} // expected-warning {{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}}
}

@available (*, deprecated) // expected-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}}
func deprecated() {}

@propertyWrapper
struct MyPropertyWrapper {
  var wrappedValue: Int = 1

  init(param: Int) {}
}

struct PropertyWrapperTest {
  @MyPropertyWrapper (param: 2)  // expected-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}}
  var x: Int
}

let closure1 = { @MainActor (a, b) in // expected-warning {{extraneous whitespace between attribute name and '('; this is an error in the Swift 6 language mode}}
}

let closure2 = { @MainActor
  (a: Int, b: Int) in
}

// expected-warning@+1{{extraneous whitespace between '@' and attribute name; this is an error in the Swift 6 language mode}}
@ 
MainActor
func mainActorFunc() {}

// RUN: %target-typecheck-verify-swift -parse-as-library \
// RUN:   -define-availability "_myProject 2.0:macOS 12.0"

// MARK: - Valid declarations

@globalConstructor func normalFunc() {}

@globalConstructor(priority: 0) func explicitVoidFunc() -> Void {}

@globalConstructor func explicitEmptyTupleFunc() -> () {}

@globalConstructor(priority: 50) private func privateFunc() {}

// MARK: - Unsupported declaration types

@globalConstructor // expected-error {{@globalConstructor cannot be applied to throwing functions}}
func throwingFunc() throws {}

@available(macOS 10.15, iOS 13, tvOS 13, watchOS 9, *)
@globalConstructor // expected-error {{@globalConstructor cannot be applied to async functions}}
func asyncFunc() async {}

@globalConstructor(priority: 0) // expected-error {{@globalConstructor may only be used on 'func' declarations}}
let closure: () -> Void = {}

@globalConstructor // expected-error {{@globalConstructor may only be used on 'func' declarations}}
class SomeClass {
  @globalConstructor // expected-error {{@globalConstructor can only be applied to global functions}}
  class func staticFunc() {}

  @globalConstructor // expected-error {{@globalConstructor can only be applied to global functions}}
  func memberFunc() {}

  @globalConstructor // expected-error {{@globalConstructor may only be used on 'func' declarations}}
  init() {}

  @globalConstructor // expected-error {{@globalConstructor may only be used on 'func' declarations}}
  deinit {}

  @globalConstructor // expected-error {{@globalConstructor may only be used on 'func' declarations}}
  subscript() -> Int {
    return 0
  }
}

@globalConstructor // expected-error {{@globalConstructor may only be used on 'func' declarations}}
class Subclass: SomeClass {
  @globalConstructor // expected-error {{@globalConstructor can only be applied to global functions}}
  override class func staticFunc() {}

  @globalConstructor // expected-error {{@globalConstructor can only be applied to global functions}}
  override func memberFunc() {}
}

@globalConstructor(priority 5) // expected-error {{expected ':' after 'priority' in '@globalConstructor' attribute}} {{28-28=: }}
func missingColon() {}

@globalConstructor(5) // expected-error {{expected 'priority:' in '@globalConstructor' attribute}} {{20-20=priority: }}
func missingPriorityLabel() {}

@globalConstructor(priority: -5) // expected-error {{expected 'priority' in '@globalConstructor' to be an integer value between 0 and 65535}} expected-error {{expected declaration}}
func negativeNumber() {}

@globalConstructor(priority: 1000000) // expected-error {{expected 'priority' in '@globalConstructor' to be an integer value between 0 and 65535}}
func largeNumber() {}

@globalConstructor(priority: 0x1) // expected-error {{expected 'priority' in '@globalConstructor' to be an integer value between 0 and 65535}} expected-error {{expected declaration}}
func hexNumber() {}

@globalConstructor(priority: "something") // expected-error {{expected 'priority' in '@globalConstructor' to be an integer value between 0 and 65535}} expected-error {{expected declaration}}
func stringInsteadOfNumber() {}

@globalConstructor(priority: 5.5) // expected-error {{expected 'priority' in '@globalConstructor' to be an integer value between 0 and 65535}} expected-error {{expected declaration}}
func floatInsteadOfInt() {}

@globalConstructor(priority: 4000) // expected-error {{'@globalConstructor' can only be applied to functions of type '() -> Void'}}
func returnsSomething() -> Int { 1 }

@globalConstructor(priority: 5000) // expected-error {{'@globalConstructor' can only be applied to functions of type '() -> Void'}}
func returnsNever() -> Never {}

@globalConstructor(priority: 6000) // expected-error {{'@globalConstructor' can only be applied to functions of type '() -> Void'}}
func takesAnArgument(_ a: Int) {}

@globalConstructor(priority: 7777) // expected-error {{'@globalConstructor' can only be applied to functions of type '() -> Void'}}
func takesAnArgumentWithDefaultValue(_ a: Int = 5) {}

@globalConstructor(priority: 50) // expected-error {{'@globalConstructor' can only be applied to functions of type '() -> Void'}}
func hasGenerics<T>(_ a: T) {}

@globalConstructor(priority: 0) // expected-error {{'@MainActor' cannot be used on functions with '@globalConstructor'}}
@MainActor
func isMainActor() {}

@globalConstructor(priority: 0) // expected-error {{'@_cdecl' cannot be used on functions with '@globalConstructor'}}
@_cdecl("some_decl")
func hasCdecl() {}

@globalConstructor(priority: 100 // expected-note {{to match this opening '('}}
func missingRightParen() {} // expected-error {{expected ')' after '@globalConstructor' priority}}

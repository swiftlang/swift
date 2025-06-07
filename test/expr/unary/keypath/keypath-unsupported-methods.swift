// RUN: %target-typecheck-verify-swift -disable-experimental-parser-round-trip

func test_keypath_with_method_refs() {
  struct S {
    func foo() -> Int { return 42 }
    static func bar() -> Int { return 0 }
  }

  let _: KeyPath<S, Int> = \.foo // expected-error {{key path cannot refer to instance method 'foo()'}}
  // expected-error@-1 {{cannot assign value of type 'KeyPath<S, () -> Int>' to type 'KeyPath<S, Int>'}}
  // expected-note@-2 {{arguments to generic parameter 'Value' ('() -> Int' and 'Int') are expected to be equal}}
  let _: KeyPath<S, Int> = \.bar // expected-error {{key path cannot refer to static method 'bar()'}}
  // expected-error@-1 {{cannot assign value of type 'KeyPath<S, () -> Int>' to type 'KeyPath<S, Int>'}}
  // expected-note@-2 {{arguments to generic parameter 'Value' ('() -> Int' and 'Int') are expected to be equal}}
  let _ = \S.Type.bar // expected-error {{key path cannot refer to static method 'bar()'}}

  struct A {
    func foo() -> B { return B() }
    static func faz() -> B { return B() }
  }

  struct B {
    var bar: Int = 42
  }

  let _: KeyPath<A, Int> = \.foo.bar // expected-error {{key path cannot refer to instance method 'foo()'}}
  let _: KeyPath<A, Int> = \.faz.bar // expected-error {{key path cannot refer to static method 'faz()'}}
  let _ = \A.foo.bar // expected-error {{key path cannot refer to instance method 'foo()'}}
  let _ = \A.Type.faz.bar // expected-error {{key path cannot refer to static method 'faz()'}}
}

// https://github.com/apple/swift/issues/54961
// Compiler crash on invalid method reference in key path.

protocol Zonk {
  func wargle()
}
typealias Blatz = (gloop: String, zoop: Zonk?)

func f_54961(fleep: [Blatz]) {
  fleep.compactMap(\.zoop?.wargle) // expected-error {{key path cannot refer to instance method 'wargle()'}}
}

// https://github.com/apple/swift/issues/56996
func f_56996() {
  _ = \Int.byteSwapped.signum() // expected-error {{key path cannot refer to instance method 'signum()'}}
  _ = \Int.byteSwapped.init() // expected-error {{key path cannot refer to initializer 'init()'}}
}

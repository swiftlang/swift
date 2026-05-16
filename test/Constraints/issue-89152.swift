// RUN: %target-typecheck-verify-swift

func outer() {
  protocol Foo {}
  
  extension Int: Foo {}
  // expected-error@-1 {{declaration is only valid at file scope}}
  // expected-error@-2 {{cannot find type 'Foo' in scope}}

  func foo(_: Int) -> some Foo { return 1738 }
  // expected-error@-1 {{return type of local function 'foo' requires that 'Int' conform to 'Foo'}}
  // expected-note@-2 {{opaque return type declared here}}
  
  func foo(_: String) -> some Foo { return 679 }
  // expected-error@-1 {{return type of local function 'foo' requires that 'Int' conform to 'Foo'}}
  // expected-note@-2 {{opaque return type declared here}}
  
  func foo<T: Foo>(_ x: T) -> some Foo { return x }

  var globalVarTuple: (some Foo, some Foo) = (123, foo(123))
  // expected-error@-1 {{return type of var 'globalVarTuple' requires that 'Int' conform to 'Foo'}}
  // expected-note@-2 {{opaque return type declared here}}
}
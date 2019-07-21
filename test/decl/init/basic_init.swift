// RUN: %target-typecheck-verify-swift -enable-objc-interop -disable-objc-attr-requires-foundation-module

class Foo {
  func bar(_: bar) {} // expected-error{{use of undeclared type 'bar'}}
}

class C {
	var triangle : triangle  // expected-error{{use of undeclared type 'triangle'}}

	init() {}
}

typealias t = t // expected-error {{type alias 't' references itself}}
// expected-note@-1{{type declared here}}

extension Foo {
  convenience init() {} // expected-error{{invalid redeclaration of synthesized 'init()'}}
}

class InitClass {
  init(arg: Bool) {} // expected-note{{add '@objc' to make this declaration overridable}}
  @objc init(baz: Int) {} // expected-note{{overridden declaration is here}}
  @objc dynamic init(bar: Int) {}
}
class InitSubclass: InitClass {}
// expected-error@-1 {{'init(bar:)' has already been overridden}}
// expected-error@-2 {{'init(baz:)' has already been overridden}}
extension InitSubclass {
  convenience init(arg: Bool) {} // expected-error{{overriding non-@objc declarations from extensions is not supported}}
  convenience override init(baz: Int) {} // expected-note{{'init(baz:)' previously overridden here}}
  // expected-error@-1 {{cannot override a non-dynamic class declaration from an extension}}
  convenience override init(bar: Int) {} // expected-note{{'init(bar:)' previously overridden here}}
}

struct InitStruct {
  let foo: Int
}
extension InitStruct {
  init(foo: Int) {} // expected-error{{invalid redeclaration of synthesized memberwise 'init(foo:)'}}
}

// <rdar://problem/17564699> QoI: Structs should get convenience initializers
struct MyStruct {
  init(k: Int) {
  }
  convenience init() {  // expected-error {{delegating initializers in structs are not marked with 'convenience'}} {{3-15=}}
    self.init(k: 1)
  }
}

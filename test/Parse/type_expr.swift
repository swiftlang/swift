// RUN: %target-parse-verify-swift

// Types in expression contexts must be followed by a member access or
// constructor call.

struct Foo {
  struct Bar {
    init() {}
    static var prop: Int = 0
    static func meth() {}
    func instMeth() {}
  }
  init() {}
  static var prop: Int = 0
  static func meth() {}
  func instMeth() {}
}

protocol Zim {
  typealias Zang

  init()
  // TODO class var prop: Int { get }
  static func meth() {} // expected-error{{protocol methods may not have bodies}}
  func instMeth() {} // expected-error{{protocol methods may not have bodies}}
}

protocol Bad {
  init() {} // expected-error{{protocol initializers may not have bodies}}
}

struct Gen<T> {
  struct Bar { // expected-error{{nested in generic type}}
    init() {}
    static var prop: Int { return 0 }
    static func meth() {}
    func instMeth() {}
  }

  init() {}
  static var prop: Int { return 0 }
  static func meth() {}
  func instMeth() {}
}

func unqualifiedType() {
  _ = Foo.self
  _ = Foo.self
  _ = Foo()
  _ = Foo.prop
  _ = Foo.meth
  let _ : () = Foo.meth()
  _ = Foo.instMeth

  _ = Foo // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  _ = Foo.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func qualifiedType() {
  _ = Foo.Bar.self
  _ = Foo.Bar()
  _ = Foo.Bar.prop
  _ = Foo.Bar.meth
  let _ : () = Foo.Bar.meth()
  _ = Foo.Bar.instMeth

  _ = Foo.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  _ = Foo.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

/* TODO allow '.Type' in expr context
func metaType() {
  let ty = Foo.Type.self
  let metaTy = Foo.Type.self

  let badTy = Foo.Type
  let badMetaTy = Foo.Type.dynamicType
}
 */

func genType() {
  _ = Gen<Foo>.self
  _ = Gen<Foo>()
  _ = Gen<Foo>.prop
  _ = Gen<Foo>.meth
  let _ : () = Gen<Foo>.meth()
  _ = Gen<Foo>.instMeth

  // Misparses because generic parameter disambiguation rejects '>' not
  // followed by '.' or '('
  _ = Gen<Foo> // expected-error{{not a postfix unary operator}}
  _ = Gen<Foo>.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func genQualifiedType() {
  _ = Gen<Foo>.Bar.self
  _ = Gen<Foo>.Bar()
  _ = Gen<Foo>.Bar.prop
  _ = Gen<Foo>.Bar.meth
  let _ : () = Gen<Foo>.Bar.meth()
  _ = Gen<Foo>.Bar.instMeth

  _ = Gen<Foo>.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  _ = Gen<Foo>.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func archetype<T: Zim>(_: T) {
  _ = T.self
  _ = T()
  // TODO let prop = T.prop
  _ = T.meth
  let _ : () = T.meth()

  _ = T // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  _ = T.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func assocType<T: Zim where T.Zang: Zim>(_: T) {
  _ = T.Zang.self
  _ = T.Zang()
  // TODO _ = T.Zang.prop
  _ = T.Zang.meth
  let _ : () = T.Zang.meth()

  _ = T.Zang // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  _ = T.Zang.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

class B {
  class func baseMethod() {}
}
class D: B {
  class func derivedMethod() {}
}

func derivedType() {
  let _: B.Type = D.self
  _ = D.baseMethod
  let _ : () = D.baseMethod()

  let _: D.Type = D.self
  _ = D.derivedMethod
  let _ : () = D.derivedMethod()

  let _: B.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _: D.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _: D.Type.Type = D.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

// Referencing a nonexistent member or constructor should not trigger errors
// about the type expression.
func nonexistentMember() {
  let cons = Foo("this constructor does not exist") // expected-error{{cannot invoke initializer for type 'Foo' with an argument list of type '(String)'}}
  let prop = Foo.nonexistent // expected-error{{does not have a member named 'nonexistent'}}
  let meth = Foo.nonexistent() // expected-error{{does not have a member named 'nonexistent'}}
}

protocol P {}

func meta_metatypes() {
  let _: P.Protocol = P.self
  _ = P.Type.self
  _ = P.Protocol.self
  _ = P.Protocol.Protocol.self // expected-error{{'Protocol' type only applies to existential types}}
  _ = P.Protocol.Type.self
  _ = B.Type.self
}

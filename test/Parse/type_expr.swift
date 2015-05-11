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
  let _ = Foo.self
  let _ = Foo.self
  let _ = Foo()
  let _ = Foo.prop
  let _ = Foo.meth
  let _ : () = Foo.meth()
  let _ = Foo.instMeth

  let _ = Foo // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _ = Foo.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func qualifiedType() {
  let _ = Foo.Bar.self
  let _ = Foo.Bar()
  let _ = Foo.Bar.prop
  let _ = Foo.Bar.meth
  let _ : () = Foo.Bar.meth()
  let _ = Foo.Bar.instMeth

  let _ = Foo.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _ = Foo.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
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
  let _ = Gen<Foo>.self
  let _ = Gen<Foo>()
  let _ = Gen<Foo>.prop
  let _ = Gen<Foo>.meth
  let _ : () = Gen<Foo>.meth()
  let _ = Gen<Foo>.instMeth

  // Misparses because generic parameter disambiguation rejects '>' not
  // followed by '.' or '('
  let _ = Gen<Foo> // expected-error{{not a postfix unary operator}}
  let _ = Gen<Foo>.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func genQualifiedType() {
  let _ = Gen<Foo>.Bar.self
  let _ = Gen<Foo>.Bar()
  let _ = Gen<Foo>.Bar.prop
  let _ = Gen<Foo>.Bar.meth
  let _ : () = Gen<Foo>.Bar.meth()
  let _ = Gen<Foo>.Bar.instMeth

  let _ = Gen<Foo>.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _ = Gen<Foo>.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func archetype<T: Zim>(_: T) {
  let _ = T.self
  let _ = T()
  // TODO let prop = T.prop
  let _ = T.meth
  let _ : () = T.meth()

  let _ = T // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _ = T.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func assocType<T: Zim where T.Zang: Zim>(_: T) {
  let _ = T.Zang.self
  let _ = T.Zang()
  // TODO let _ = T.Zang.prop
  let _ = T.Zang.meth
  let _ : () = T.Zang.meth()

  let _ = T.Zang // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let _ = T.Zang.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

class B {
  class func baseMethod() {}
}
class D: B {
  class func derivedMethod() {}
}

func derivedType() {
  let _: B.Type = D.self
  let _ = D.baseMethod
  let _ : () = D.baseMethod()

  let _: D.Type = D.self
  let _ = D.derivedMethod
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
  let _ = P.Type.self
  let _ = P.Protocol.self
  let _ = P.Protocol.Protocol.self // expected-error{{'Protocol' type only applies to existential types}}
  let _ = P.Protocol.Type.self
  let _ = B.Type.self
}

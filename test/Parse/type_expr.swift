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
  let ty = Foo.self
  let metaTy = Foo.self
  let cons = Foo()
  let prop = Foo.prop
  let clos = Foo.meth
  let meth : () = Foo.meth()
  let inst = Foo.instMeth

  let badTy = Foo // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy = Foo.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func qualifiedType() {
  let ty = Foo.Bar.self
  let cons = Foo.Bar()
  let prop = Foo.Bar.prop
  let clos = Foo.Bar.meth
  let meth : () = Foo.Bar.meth()
  let inst = Foo.Bar.instMeth

  let badTy = Foo.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy = Foo.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
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
  let ty = Gen<Foo>.self
  let cons = Gen<Foo>()
  let prop = Gen<Foo>.prop
  let clos = Gen<Foo>.meth
  let meth : () = Gen<Foo>.meth()
  let inst = Gen<Foo>.instMeth

  // Misparses because generic parameter disambiguation rejects '>' not
  // followed by '.' or '('
  let badTy = Gen<Foo> // expected-error{{not a postfix unary operator}}
  let badMetaTy = Gen<Foo>.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func genQualifiedType() {
  let ty = Gen<Foo>.Bar.self
  let cons = Gen<Foo>.Bar()
  let prop = Gen<Foo>.Bar.prop
  let clos = Gen<Foo>.Bar.meth
  let meth : () = Gen<Foo>.Bar.meth()
  let inst = Gen<Foo>.Bar.instMeth

  let badTy = Gen<Foo>.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy = Gen<Foo>.Bar.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func archetype<T: Zim>(_: T) {
  let ty = T.self
  let cons = T()
  // TODO let prop = T.prop
  let clos = T.meth
  let meth : () = T.meth()

  let badTy = T // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy = T.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

func assocType<T: Zim where T.Zang: Zim>(_: T) {
  let ty = T.Zang.self
  let cons = T.Zang()
  // TODO let prop = T.Zang.prop
  let clos = T.Zang.meth
  let meth : () = T.Zang.meth()

  let badTy = T.Zang // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy = T.Zang.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
}

class B {
  class func baseMethod() {}
}
class D: B {
  class func derivedMethod() {}
}

func derivedType() {
  let baseTy: B.Type = D.self
  let baseClos = D.baseMethod
  let baseMeth : () = D.baseMethod()

  let derivedTy: D.Type = D.self
  let derivedClos = D.derivedMethod
  let derivedMeth : () = D.derivedMethod()

  let badBaseTy: B.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badDerivedTy: D.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} expected-note{{use '.self'}}
  let badMetaTy: D.Type.Type = D.dynamicType // expected-error{{'.dynamicType' is not allowed after a type name}}
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
  let p: P.Protocol = P.self
  let a = P.Type.self
  let b = P.Protocol.self
  let c = P.Protocol.Protocol.self // expected-error{{'Protocol' type only applies to existential types}}
  let d = P.Protocol.Type.self
  let e = B.Type.self
}

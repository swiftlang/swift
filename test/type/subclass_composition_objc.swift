// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module -enable-objc-interop

@objc class ObjCClass {}
@objc protocol ObjCProtocol {}

class SwiftClass {}
protocol SwiftProtocol {}

class SomeMethods {

  @objc
  func canRepresentInObjC(x: ObjCClass & ObjCProtocol) {}

  @objc
  func cannotRepresentInObjC(x: SwiftClass & ObjCProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol-constrained type containing class 'SwiftClass' cannot be represented in Objective-C}}

  @objc
  func alsoCannotRepresentInObjC(x: ObjCClass & SwiftProtocol) {}
  // expected-error@-1 {{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-2 {{protocol-constrained type containing protocol 'SwiftProtocol' cannot be represented in Objective-C}}
}

// Test self-conformance

func takesObjCClass<T : ObjCClass>(_: T) {}
func takesObjCProtocol<T : ObjCProtocol>(_: T) {}
func takesObjCClassAndProtocol<T : ObjCClass & ObjCProtocol>(_: T) {}
// expected-note@-1 2 {{in call to function 'takesObjCClassAndProtocol'}}

func testSelfConformance(c: ObjCClass, p: ObjCProtocol, cp: ObjCClass & ObjCProtocol) {
  takesObjCClass(c)
  takesObjCClass(p) // expected-error {{cannot convert value of type 'ObjCProtocol' to expected argument type 'ObjCClass'}}
  takesObjCClass(cp)

  takesObjCProtocol(c) // expected-error {{argument type 'ObjCClass' does not conform to expected type 'ObjCProtocol'}}
  takesObjCProtocol(p)
  takesObjCProtocol(cp)

  // FIXME: Bad diagnostics
  takesObjCClassAndProtocol(c) // expected-error {{generic parameter 'T' could not be inferred}}
  takesObjCClassAndProtocol(p) // expected-error {{generic parameter 'T' could not be inferred}}
  takesObjCClassAndProtocol(cp)
}

@objc protocol StaticObjCProtocol {
  static func f()
}

func takesStaticObjCProtocol<T : StaticObjCProtocol>(_: T) {}

func testSelfConformance(cp: ObjCClass & StaticObjCProtocol) {

  // FIXME: Terrible diagnostic
  takesStaticObjCProtocol(cp)
  // expected-error@-1 {{cannot invoke 'takesStaticObjCProtocol' with an argument list of type '(ObjCClass & StaticObjCProtocol)'}}
  // expected-note@-2 {{expected an argument list of type '(T)'}}

}

func testMetatypeSelfConformance(m1: (ObjCClass & ObjCProtocol).Protocol,
                                 m2: (ObjCClass & StaticObjCProtocol).Protocol) {
  _ = m1 as (ObjCClass & ObjCProtocol).Type
  _ = m1 as? (ObjCClass & ObjCProtocol).Type // expected-warning {{always succeeds}}

  _ = m2 as (ObjCClass & StaticObjCProtocol).Type // expected-error {{'(ObjCClass & StaticObjCProtocol).Protocol' is not convertible to '(ObjCClass & StaticObjCProtocol).Type'; did you mean to use 'as!' to force downcast?}}
  _ = m2 as? (ObjCClass & StaticObjCProtocol).Type // FIXME should 'always fail'
}

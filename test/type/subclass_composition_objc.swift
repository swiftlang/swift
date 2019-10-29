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

func takesObjCClass<T : ObjCClass>(_: T) {} // expected-note {{where 'T' = 'ObjCProtocol'}}
func takesObjCProtocol<T : ObjCProtocol>(_: T) {} // expected-note {{where 'T' = 'ObjCClass'}}
func takesObjCClassAndProtocol<T : ObjCClass & ObjCProtocol>(_: T) {} // expected-note {{where 'T' = 'ObjCProtocol'}} expected-note {{where 'T' = 'ObjCClass'}}

func testSelfConformance(c: ObjCClass, p: ObjCProtocol, cp: ObjCClass & ObjCProtocol) {
  takesObjCClass(c)
  takesObjCClass(p) // expected-error {{global function 'takesObjCClass' requires that 'ObjCProtocol' inherit from 'ObjCClass'}}
  takesObjCClass(cp)

  takesObjCProtocol(c) // expected-error {{global function 'takesObjCProtocol' requires that 'ObjCClass' conform to 'ObjCProtocol'}}
  takesObjCProtocol(p)
  takesObjCProtocol(cp)

  // FIXME: Bad diagnostics
  takesObjCClassAndProtocol(c) // expected-error {{global function 'takesObjCClassAndProtocol' requires that 'ObjCClass' conform to 'ObjCProtocol'}}
  takesObjCClassAndProtocol(p) // expected-error {{global function 'takesObjCClassAndProtocol' requires that 'ObjCProtocol' inherit from 'ObjCClass'}}
  takesObjCClassAndProtocol(cp)
}

@objc protocol StaticObjCProtocol {
  static func f()
}

func takesStaticObjCProtocol<T : StaticObjCProtocol>(_: T) {}

func testSelfConformance(cp: ObjCClass & StaticObjCProtocol) {
  takesStaticObjCProtocol(cp)
  // expected-error@-1 {{'ObjCClass & StaticObjCProtocol' cannot be used as a type conforming to protocol 'StaticObjCProtocol' because 'StaticObjCProtocol' has static requirements}}
}

func testMetatypeSelfConformance(m1: (ObjCClass & ObjCProtocol).Protocol,
                                 m2: (ObjCClass & StaticObjCProtocol).Protocol) {
  _ = m1 as (ObjCClass & ObjCProtocol).Type
  _ = m1 as? (ObjCClass & ObjCProtocol).Type // expected-warning {{always succeeds}}

  _ = m2 as (ObjCClass & StaticObjCProtocol).Type // expected-error {{cannot convert value of type '(ObjCClass & StaticObjCProtocol).Protocol' to type '(ObjCClass & StaticObjCProtocol).Type' in coercion}}
  _ = m2 as? (ObjCClass & StaticObjCProtocol).Type // FIXME should 'always fail'
}

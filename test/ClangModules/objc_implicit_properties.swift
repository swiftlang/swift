// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -I %S/Inputs/custom-modules -enable-objc-implicit-properties %s -verify

// REQUIRES: objc_interop

import ObjectiveC
import ObjCImplicitProperties

func implicitProperties(obj: ImplicitProperties, other: AnyObject) {
  obj.implicitProperty = other // no-warning
  obj.implicitProperty.hash // no-warning

  obj.anotherImplicitProperty = 42 // no-warning
  println(Int(obj.anotherImplicitProperty)) // no-warning
}

func acceptsInt(_: Int) {}
func acceptsInt(_: UInt) {}

func badImplicitProperties(obj: BadImplicitProperties) {
  acceptsInt(obj.nonVoidReturn) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '(() -> Int32)'}}
  acceptsInt(obj.nonMatchingType) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '(() -> Int32)'}}
  acceptsInt(obj.wrongGetterArgs) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '((Int32) -> Int32)'}}
  acceptsInt(obj.wrongSetterArgs) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '(() -> Int32)'}}
  acceptsInt(obj.wrongSetterArgs2) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '(() -> Int32)'}}
  acceptsInt(obj.getterOnly) // expected-error {{cannot find an overload for 'acceptsInt' that accepts an argument list of type '(() -> Int32)'}}
  acceptsInt(obj.setterOnly) // expected-error {{'BadImplicitProperties' does not have a member named 'setterOnly'}}

  // But we should still import all of the methods as methods.
  let x: CInt = obj.setNonVoidReturn(obj.nonVoidReturn())
  let y: CInt = obj.nonMatchingType()
  obj.setNonMatchingType(obj)
  obj.setWrongGetterArgs(obj.wrongGetterArgs(42))
  obj.setWrongSetterArgs(obj.wrongSetterArgs(), extra: 42)
  let z: CInt = obj.wrongSetterArgs2()
  obj.setWrongSetterArgs2()
  obj.setSetterOnly(obj.getterOnly())
}

func overriding(obj: Sub) {
  let a: AnyObject = obj.methodInBase()
  let b: AnyObject = obj.propertyInBase
  let c: AnyObject = obj.methodPairInBase
  let d: AnyObject = obj.getterOnlyInBase // expected-error {{}}
  let e: AnyObject = obj.setterOnlyInBase // expected-error {{}}
  let f: AnyObject = obj.methodInProto
  let g: AnyObject = obj.propertyInProto()
  let h: AnyObject = obj.methodInBaseButPropertyInProto()
  let i: AnyObject = obj.propertyInBaseButMethodInProto

  obj.setGetterOnlyInBase(obj.getterOnlyInBase())
  obj.setSetterOnlyInBase(obj.setterOnlyInBase())

  // FIXME: These are incorrectly accepted or incorrectly rejected.
  obj.setMethodPairInBase(c)

  obj.setPropertyInProto(g) // expected-error {{does not have a member}}
  obj.setMethodInBaseButPropertyInProto(h) // expected-error {{does not have a member}}
}

func doSomethingBase<T: PropertiesProto>(obj: T) {}
func doSomethingSub<T: SubProto>(obj: T) {}

func protocols(obj: Sub) {
  let baseProto: PropertiesProto = obj
  let subProto: SubProto = obj
  doSomethingBase(obj)
  doSomethingSub(obj)
}

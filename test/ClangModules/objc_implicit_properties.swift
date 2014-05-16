// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -I %S/Inputs/custom-modules -enable-objc-implicit-properties %s -verify

import ObjectiveC
import ObjCImplicitProperties

func implicitProperties(obj: ImplicitProperties, other: AnyObject) {
  obj.implicitProperty = other // no-warning
  obj.implicitProperty.description // no-warning

  obj.anotherImplicitProperty = 42 // no-warning
  println(Int(obj.anotherImplicitProperty)) // no-warning
}

func acceptsInt(_: Int) {}
func acceptsInt(_: UInt) {}

func badImplicitProperties(obj: BadImplicitProperties) {
  acceptsInt(obj.nonVoidReturn) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
  acceptsInt(obj.nonMatchingType) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
  acceptsInt(obj.wrongGetterArgs) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
  acceptsInt(obj.wrongSetterArgs) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
  acceptsInt(obj.wrongSetterArgs2) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
  acceptsInt(obj.getterOnly) // expected-error {{could not find an overload for 'acceptsInt' that accepts the supplied arguments}}
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

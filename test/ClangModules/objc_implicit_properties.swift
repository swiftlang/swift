// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -I %S/Inputs/custom-modules -enable-objc-implicit-properties %s -verify

// REQUIRES: objc_interop

import ObjectiveC
import ObjCImplicitProperties

func acceptsInt(_: Int) {}
func acceptsInt(_: UInt) {}

func implicitProperties(obj: ImplicitProperties, other: AnyObject) {
  obj.implicitProperty = other // no-warning
  obj.implicitProperty.hash // no-warning

  obj.anotherImplicitProperty = 42 // no-warning
  acceptsInt(Int(obj.anotherImplicitProperty)) // no-warning
}

func badImplicitProperties(obj: BadImplicitProperties) {
  acceptsInt(obj.nonVoidReturn) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '(() -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}
  
  acceptsInt(obj.nonMatchingType) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '(() -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}
  
  acceptsInt(obj.wrongGetterArgs) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '((Int32) -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}

  acceptsInt(obj.wrongSetterArgs) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '(() -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}

  acceptsInt(obj.wrongSetterArgs2) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '(() -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}

  acceptsInt(obj.getterOnly) // expected-error {{cannot invoke 'acceptsInt' with an argument list of type '(() -> Int32)'}}
  // expected-note @-1 {{overloads for 'acceptsInt' exist with these partially matching parameter lists: (Int), (UInt)}}

  acceptsInt(obj.setterOnly) // expected-error {{value of type 'BadImplicitProperties' has no member 'setterOnly'}}

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
  let d: AnyObject = obj.getterOnlyInBase // expected-error {{}} {{42-42=()}}
  let e: AnyObject = obj.setterOnlyInBase // expected-error {{}} {{42-42=()}}
  let f: AnyObject = obj.methodInProto
  let g: AnyObject = obj.propertyInProto()
  let h: AnyObject = obj.methodInBaseButPropertyInProto()
  let i: AnyObject = obj.propertyInBaseButMethodInProto

  obj.setGetterOnlyInBase(obj.getterOnlyInBase())
  obj.setSetterOnlyInBase(obj.setterOnlyInBase())

  // FIXME: These are incorrectly accepted or incorrectly rejected.
  obj.setMethodPairInBase(c)

  obj.setPropertyInProto(g) // expected-error {{value of type 'Sub' has no member 'setPropertyInProto'}}
  obj.setMethodInBaseButPropertyInProto(h) // expected-error {{value of type 'Sub' has no member 'setMethodInBaseButPropertyInProto'}}
}

func doSomethingBase<T: PropertiesProto>(obj: T) {}
func doSomethingSub<T: SubProto>(obj: T) {}

func protocols(obj: Sub) {
  _ = obj as PropertiesProto
  _ = obj as SubProto
  doSomethingBase(obj)
  doSomethingSub(obj)
}

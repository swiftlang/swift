// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | FileCheck %s
// REQUIRES: objc_interop

import objc_generics

func callInitializer() {
  _ = GenericClass(thing: NSObject())
}

// CHECK-LABEL: sil shared @_TFCSo12GenericClassCfT5thingGSQx__GSQGS_x__
// CHECK:         thick_to_objc_metatype {{%.*}} : $@thick GenericClass<T>.Type to $@objc_metatype GenericClass<T>.Type

public func genericMethodOnAnyObject(o: AnyObject, b: Bool) -> AnyObject {
  return o.thing!()!
}

// CHECK-LABEL: sil @_TF21objc_imported_generic24genericMethodOnAnyObject
// CHECK:         dynamic_method [volatile] {{%.*}} : $@opened([[TAG:.*]]) AnyObject, #GenericClass.thing!1.foreign : <T where T : AnyObject> GenericClass<T> -> () -> T?, $@convention(objc_method) (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>

public func genericMethodOnAnyObjectChained(o: AnyObject, b: Bool) -> AnyObject? {
  return o.thing?()
}

// CHECK-LABEL: sil @_TF21objc_imported_generic31genericMethodOnAnyObjectChained
// CHECK:         dynamic_method_br %4 : $@opened([[TAG:.*]]) AnyObject, #GenericClass.thing!1.foreign, bb1
// CHECK:       bb1({{%.*}} : $@convention(objc_method) (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>):

public func genericSubscriptOnAnyObject(o: AnyObject, b: Bool) -> AnyObject? {
  return o[0 as UInt16]
}

// CHECK-LABEL: sil @_TF21objc_imported_generic27genericSubscriptOnAnyObject
// CHECK:         dynamic_method_br %4 : $@opened([[TAG:.*]]) AnyObject, #GenericClass.subscript!getter.1.foreign, bb1
// CHECK:       bb1({{%.*}} : $@convention(objc_method) (UInt16, @opened([[TAG]]) AnyObject) -> @autoreleased AnyObject):

public func genericPropertyOnAnyObject(o: AnyObject, b: Bool) -> AnyObject?? {
  return o.propertyThing
}

// CHECK-LABEL: sil @_TF21objc_imported_generic26genericPropertyOnAnyObject
// CHECK:         dynamic_method_br %4 : $@opened([[TAG:.*]]) AnyObject, #GenericClass.propertyThing!getter.1.foreign, bb1
// CHECK:       bb1({{%.*}} : $@convention(objc_method) (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>):

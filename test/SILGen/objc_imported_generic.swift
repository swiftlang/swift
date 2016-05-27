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

protocol ThingHolder {
  associatedtype Thing

  init!(thing: Thing!)
  func thing() -> Thing?
  func arrayOfThings() -> [Thing]
  func setArrayOfThings(_: [Thing])
  static func classThing() -> Thing?

  var propertyThing: Thing? { get set }
  var propertyArrayOfThings: [Thing]? { get set }
}

extension GenericClass: ThingHolder {}

// CHECK-LABEL: sil @_TF21objc_imported_generic26genericPropertyOnAnyObject
// CHECK:         dynamic_method_br %4 : $@opened([[TAG:.*]]) AnyObject, #GenericClass.propertyThing!getter.1.foreign, bb1
// CHECK:       bb1({{%.*}} : $@convention(objc_method) (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>):

// CHECK-LABEL: sil @_TF21objc_imported_generic20arraysOfGenericParam
public func arraysOfGenericParam<T: AnyObject>(y: Array<T>) {
  // CHECK:         function_ref {{@_TFCSo12GenericClassCfT13arrayOfThings.*}} : $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@owned Array<τ_0_0>, @thick GenericClass<τ_0_0>.Type) -> @owned ImplicitlyUnwrappedOptional<GenericClass<τ_0_0>>
  let x = GenericClass<T>(arrayOfThings: y)!
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.setArrayOfThings!1.foreign {{.*}}, $@convention(objc_method) <τ_0_0 where τ_0_0 : AnyObject> (NSArray, GenericClass<τ_0_0>) -> ()
  x.setArrayOfThings(y)
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.propertyArrayOfThings!getter.1.foreign {{.*}}, $@convention(objc_method) <τ_0_0 where τ_0_0 : AnyObject> (GenericClass<τ_0_0>) -> @autoreleased Optional<NSArray>
  _ = x.propertyArrayOfThings
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.propertyArrayOfThings!setter.1.foreign {{.*}}, $@convention(objc_method) <τ_0_0 where τ_0_0 : AnyObject> (Optional<NSArray>, GenericClass<τ_0_0>) -> ()
  x.propertyArrayOfThings = y
}

// CHECK-LABEL: sil shared [thunk] @_TTOFCSo12GenericClasscfT13arrayOfThings
// CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.init!initializer.1.foreign {{.*}}, $@convention(objc_method) <τ_0_0 where τ_0_0 : AnyObject> (NSArray, @owned GenericClass<τ_0_0>) -> @owned ImplicitlyUnwrappedOptional<GenericClass<τ_0_0>>

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
// For integration testing, ensure we get through IRGen too.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -verify -DIRGEN_INTEGRATION_TEST %s

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
// CHECK:         dynamic_method [volatile] {{%.*}} : $@opened([[TAG:.*]]) AnyObject, #GenericClass.thing!1.foreign : <T where T : AnyObject> (GenericClass<T>) -> () -> T?, $@convention(objc_method) @pseudogeneric (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>

public func genericMethodOnAnyObjectChained(o: AnyObject, b: Bool) -> AnyObject? {
  return o.thing?()
}

// CHECK-LABEL: sil @_TF21objc_imported_generic31genericMethodOnAnyObjectChainedFT1oPs9AnyObject_1bSb_GSqPS0___
// CHECK: bb0([[ANY:%.*]] : $AnyObject, [[BOOL:%.*]] : $Bool):
// CHECK:   [[BORROWED_ANY:%.*]] = begin_borrow [[ANY]]
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[BORROWED_ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened([[TAG:.*]]) AnyObject, #GenericClass.thing!1.foreign, bb1
// CHECK:   bb1({{%.*}} : $@convention(objc_method) @pseudogeneric (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>):
// CHECK: } // end sil function '_TF21objc_imported_generic31genericMethodOnAnyObjectChainedFT1oPs9AnyObject_1bSb_GSqPS0___'

public func genericSubscriptOnAnyObject(o: AnyObject, b: Bool) -> AnyObject? {
  return o[0 as UInt16]
}

// CHECK-LABEL: sil @_TF21objc_imported_generic27genericSubscriptOnAnyObjectFT1oPs9AnyObject_1bSb_GSqPS0___
// CHECK: bb0([[ANY:%.*]] : $AnyObject,
// CHECK:   [[BORROWED_ANY:%.*]] = begin_borrow [[ANY]]
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[BORROWED_ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened([[TAG:.*]]) AnyObject, #GenericClass.subscript!getter.1.foreign, bb1
// CHECK:   bb1({{%.*}} : $@convention(objc_method) @pseudogeneric (UInt16, @opened([[TAG]]) AnyObject) -> @autoreleased AnyObject):
// CHECK: } // end sil function '_TF21objc_imported_generic27genericSubscriptOnAnyObjectFT1oPs9AnyObject_1bSb_GSqPS0___'

public func genericPropertyOnAnyObject(o: AnyObject, b: Bool) -> AnyObject?? {
  return o.propertyThing
}

// CHECK-LABEL: sil @_TF21objc_imported_generic26genericPropertyOnAnyObjectFT1oPs9AnyObject_1bSb_GSqGSqPS0____
// CHECK: bb0([[ANY:%.*]] : $AnyObject, [[BOOL:%.*]] : $Bool):
// CHECK:   [[BORROWED_ANY:%.*]] = begin_borrow [[ANY]]
// CHECK:   [[OPENED_ANY:%.*]] = open_existential_ref [[BORROWED_ANY]]
// CHECK:   [[OPENED_ANY_COPY:%.*]] = copy_value [[OPENED_ANY]]
// CHECK:   dynamic_method_br [[OPENED_ANY_COPY]] : $@opened([[TAG:.*]]) AnyObject, #GenericClass.propertyThing!getter.1.foreign, bb1
// CHECK:   bb1({{%.*}} : $@convention(objc_method) @pseudogeneric (@opened([[TAG]]) AnyObject) -> @autoreleased Optional<AnyObject>):
// CHECK: } // end sil function '_TF21objc_imported_generic26genericPropertyOnAnyObjectFT1oPs9AnyObject_1bSb_GSqGSqPS0____'

public protocol ThingHolder {
  associatedtype Thing

  init!(thing: Thing!)
  func thing() -> Thing?
  func arrayOfThings() -> [Thing]
  func setArrayOfThings(_: [Thing])
  static func classThing() -> Thing?

  var propertyThing: Thing? { get set }
  var propertyArrayOfThings: [Thing]? { get set }
}

// TODO: Crashes in IRGen because the type metadata for `T` is not found in
// the witness thunk to satisfy the associated type requirement. This could be
// addressed by teaching IRGen to fulfill erased type parameters from protocol
// witness tables (rdar://problem/26602097).
#if !IRGEN_INTEGRATION_TEST
extension GenericClass: ThingHolder {}
#endif

public protocol Ansible: class {
  associatedtype Anser: ThingHolder
}

public func genericBlockBridging<T: Ansible>(x: GenericClass<T>) {
  let block = x.blockForPerformingOnThings()
  x.performBlock(onThings: block)
}

// CHECK-LABEL: sil @_TF21objc_imported_generic20genericBlockBridging
// CHECK:         [[BLOCK_TO_FUNC:%.*]] = function_ref @_TTRGRxs9AnyObjectx21objc_imported_generic7AnsiblerXFdCb_dx_ax_XFo_ox_ox_
// CHECK:         partial_apply [[BLOCK_TO_FUNC]]<T>
// CHECK:         [[FUNC_TO_BLOCK:%.*]] = function_ref @_TTRgRxs9AnyObjectx21objc_imported_generic7AnsiblerXFo_ox_ox_XFdCb_dx_ax_
// CHECK:         init_block_storage_header {{.*}} invoke [[FUNC_TO_BLOCK]]<T>

// CHECK-LABEL: sil @_TF21objc_imported_generic20arraysOfGenericParam
public func arraysOfGenericParam<T: AnyObject>(y: Array<T>) {
  // CHECK:         function_ref {{@_TFCSo12GenericClassCfT13arrayOfThings.*}} : $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@owned Array<τ_0_0>, @thick GenericClass<τ_0_0>.Type) -> @owned Optional<GenericClass<τ_0_0>>
  let x = GenericClass<T>(arrayOfThings: y)!
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.setArrayOfThings!1.foreign {{.*}}, $@convention(objc_method) @pseudogeneric <τ_0_0 where τ_0_0 : AnyObject> (NSArray, GenericClass<τ_0_0>) -> ()
  x.setArrayOfThings(y)
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.propertyArrayOfThings!getter.1.foreign {{.*}}, $@convention(objc_method) @pseudogeneric <τ_0_0 where τ_0_0 : AnyObject> (GenericClass<τ_0_0>) -> @autoreleased Optional<NSArray>
  _ = x.propertyArrayOfThings
  // CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.propertyArrayOfThings!setter.1.foreign {{.*}}, $@convention(objc_method) @pseudogeneric <τ_0_0 where τ_0_0 : AnyObject> (Optional<NSArray>, GenericClass<τ_0_0>) -> ()
  x.propertyArrayOfThings = y
}

// CHECK-LABEL: sil shared @_TFF21objc_imported_generic11genericFuncuRxs9AnyObjectrFMxT_U_FT_T_ : $@convention(thin) <V where V : AnyObject> () -> () {
// CHECK:  [[INIT:%.*]] = function_ref @_TFCSo12GenericClassCfT_GS_x_ : $@convention(method) <τ_0_0 where τ_0_0 : AnyObject> (@thick GenericClass<τ_0_0>.Type) -> @owned GenericClass<τ_0_0>
// CHECK:  [[META:%.*]] = metatype $@thick GenericClass<V>.Type
// CHECK:  apply [[INIT]]<V>([[META]])
// CHECK:  return
func genericFunc<V: AnyObject>(_ v: V.Type) {
  let _ = {
    var _ = GenericClass<V>()
  }
}

// CHECK-LABEL: sil hidden @_TF21objc_imported_generic23configureWithoutOptionsFT_T_ : $@convention(thin) () -> ()
// CHECK: [[NIL_FN:%.*]] = function_ref @_TFSqCfT10nilLiteralT__GSqx_ : $@convention(method) <τ_0_0> (@thin Optional<τ_0_0>.Type) -> @out Optional<τ_0_0>
// CHECK: apply [[NIL_FN]]<[GenericOption : Any]>({{.*}})
// CHECK: return
func configureWithoutOptions() {
  _ = GenericClass<NSObject>(options: nil)
}

// foreign to native thunk for init(options:), uses GenericOption : Hashable
// conformance

// CHECK-LABEL: sil shared [thunk] @_TTOFCSo12GenericClasscfT7optionsGSqGVs10DictionaryVSC13GenericOptionP____GSQGS_x__ : $@convention(method) <T where T : AnyObject> (@owned Optional<Dictionary<GenericOption, Any>>, @owned GenericClass<T>) -> @owned Optional<GenericClass<T>>
// CHECK: [[FN:%.*]] = function_ref @_TFE10FoundationVs10Dictionary19_bridgeToObjectiveCfT_CSo12NSDictionary : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
// CHECK: apply [[FN]]<GenericOption, Any>({{.*}}) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
// CHECK: return

// This gets emitted down here for some reason

// CHECK-LABEL: sil shared [thunk] @_TTOFCSo12GenericClasscfT13arrayOfThings
// CHECK:         class_method [volatile] {{%.*}} : $GenericClass<T>, #GenericClass.init!initializer.1.foreign {{.*}}, $@convention(objc_method) @pseudogeneric <τ_0_0 where τ_0_0 : AnyObject> (NSArray, @owned GenericClass<τ_0_0>) -> @owned Optional<GenericClass<τ_0_0>>

// Make sure we emitted the witness table for the above conformance

// CHECK-LABEL: sil_witness_table shared [fragile] GenericOption: Hashable module objc_generics {
// CHECK: method #Hashable.hashValue!getter.1: {{.*}} : @_TTWVSC13GenericOptions8Hashable13objc_genericsFS0_g9hashValueSi
// CHECK: }

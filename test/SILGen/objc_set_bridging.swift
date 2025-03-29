
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-print-types -module-name objc_set_bridging %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

@objc class Foo : NSObject {
  // Bridging set parameters
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC16bridge_Set_param{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (NSSet, Foo) -> ()
  @objc func bridge_Set_param(_ s: Set<Foo>) {
    // CHECK: bb0([[NSSET:%[0-9]+]] : @unowned $NSSet, [[SELF:%[0-9]+]] : @unowned $Foo):
    // CHECK:   [[NSSET_COPY:%.*]] = copy_value [[NSSET]] : $NSSet
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Foo
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSh10FoundationE36_unconditionallyBridgeFromObjectiveCyShyxGSo5NSSetCSgFZ
    // CHECK:   [[OPT_NSSET:%[0-9]+]] = enum $Optional<NSSet>, #Optional.some!enumelt, [[NSSET_COPY]] : $NSSet
    // CHECK:   [[SET_META:%[0-9]+]] = metatype $@thin Set<Foo>.Type
    // CHECK:   [[SET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[OPT_NSSET]], [[SET_META]])
    // CHECK:   [[BORROWED_SET:%.*]] = begin_borrow [[SET]]
    // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s17objc_set_bridging3FooC16bridge_Set_param{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed Set<Foo>, @guaranteed Foo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_SET]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Set<Foo>, @guaranteed Foo) -> ()
    // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK:   return [[RESULT]] : $()
  }
  // CHECK: // end sil function '$s17objc_set_bridging3FooC16bridge_Set_param{{[_0-9a-zA-Z]*}}FTo'

  // Bridging set results
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC17bridge_Set_result{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (Foo) -> @autoreleased NSSet {
  @objc func bridge_Set_result() -> Set<Foo> { 
    // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $Foo):
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Foo
    // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s17objc_set_bridging3FooC17bridge_Set_result{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed Foo) -> @owned Set<Foo>
    // CHECK:   [[SET:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Foo) -> @owned Set<Foo>
    // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSh10FoundationE19_bridgeToObjectiveCSo5NSSetCyF
    // CHECK:   [[BORROWED_SET:%.*]] = begin_borrow [[SET]]
    // CHECK:   [[NSSET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[BORROWED_SET]]) : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned NSSet
    // CHECK:   end_borrow [[BORROWED_SET]]
    // CHECK:   destroy_value [[SET]]
    // CHECK:   return [[NSSET]] : $NSSet
  }
  // CHECK: } // end sil function '$s17objc_set_bridging3FooC17bridge_Set_result{{[_0-9a-zA-Z]*}}FTo'

  @objc var property: Set<Foo> = Set()

  // Property getter
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC8property{{[_0-9a-zA-Z]*}}vgTo : $@convention(objc_method) (Foo) -> @autoreleased NSSet
  // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $Foo):
  // CHECK:   [[SELF_COPY]] = copy_value [[SELF]] : $Foo
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[GETTER:%[0-9]+]] = function_ref @$s17objc_set_bridging3FooC8property{{[_0-9a-zA-Z]*}}vg : $@convention(method) (@guaranteed Foo) -> @owned Set<Foo>
  // CHECK:   [[SET:%[0-9]+]] = apply [[GETTER]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Foo) -> @owned Set<Foo>
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSh10FoundationE19_bridgeToObjectiveCSo5NSSetCyF
  // CHECK:   [[BORROWED_SET:%.*]] = begin_borrow [[SET]]
  // CHECK:   [[NSSET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[BORROWED_SET]]) : $@convention(method) <τ_0_0 where τ_0_0 : Hashable> (@guaranteed Set<τ_0_0>) -> @owned NSSet
  // CHECK:   end_borrow [[BORROWED_SET]]
  // CHECK:   destroy_value [[SET]]
  // CHECK:   return [[NSSET]] : $NSSet
  // CHECK: } // end sil function '$s17objc_set_bridging3FooC8property{{[_0-9a-zA-Z]*}}vgTo'
  
  // Property setter
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC8property{{[_0-9a-zA-Z]*}}vsTo : $@convention(objc_method) (NSSet, Foo) -> () {
  // CHECK: bb0([[NSSET:%[0-9]+]] : @unowned $NSSet, [[SELF:%[0-9]+]] : @unowned $Foo):
  // CHECK:   [[NSSET_COPY:%.*]] = copy_value [[NSSET]] : $NSSet
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Foo
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSh10FoundationE36_unconditionallyBridgeFromObjectiveCyShyxGSo5NSSetCSgFZ
  // CHECK:   [[OPT_NSSET:%[0-9]+]] = enum $Optional<NSSet>, #Optional.some!enumelt, [[NSSET_COPY]] : $NSSet
  // CHECK:   [[SET_META:%[0-9]+]] = metatype $@thin Set<Foo>.Type
  // CHECK:   [[SET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[OPT_NSSET]], [[SET_META]])
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SETTER:%[0-9]+]] = function_ref @$s17objc_set_bridging3FooC8property{{[_0-9a-zA-Z]*}}vs : $@convention(method) (@owned Set<Foo>, @guaranteed Foo) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SETTER]]([[SET]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@owned Set<Foo>, @guaranteed Foo) -> ()
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]] : $Foo
  // CHECK:   return [[RESULT]] : $()
  
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC19nonVerbatimProperty{{[_0-9a-zA-Z]*}}vgTo : $@convention(objc_method) (Foo) -> @autoreleased NSSet
  // CHECK-LABEL: sil private [thunk] [ossa] @$s17objc_set_bridging3FooC19nonVerbatimProperty{{[_0-9a-zA-Z]*}}vsTo : $@convention(objc_method) (NSSet, Foo) -> () {
  @objc var nonVerbatimProperty: Set<String> = Set()
}

func ==(x: Foo, y: Foo) -> Bool { }

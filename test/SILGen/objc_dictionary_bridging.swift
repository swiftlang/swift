
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-print-types -module-name objc_dictionary_bridging %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

@objc class Foo : NSObject {
  // Bridging dictionary parameters
  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC23bridge_Dictionary_param{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (NSDictionary, Foo) -> ()
  @objc func bridge_Dictionary_param(_ dict: Dictionary<Foo, Foo>) {
    // CHECK: bb0([[NSDICT:%[0-9]+]] : @unowned $NSDictionary, [[SELF:%[0-9]+]] : @unowned $Foo):
    // CHECK:   [[NSDICT_COPY:%.*]] = copy_value [[NSDICT]]
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSD10FoundationE36_unconditionallyBridgeFromObjectiveCySDyxq_GSo12NSDictionaryCSgFZ
    // CHECK:   [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.some!enumelt, [[NSDICT_COPY]] : $NSDictionary
    // CHECK:   [[DICT_META:%[0-9]+]] = metatype $@thin Dictionary<Foo, Foo>.Type
    // CHECK:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]], [[DICT_META]])
    // CHECK:   [[BORROWED_DICT:%.*]] = begin_borrow [[DICT]]
    // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s24objc_dictionary_bridging3FooC23bridge_Dictionary_param{{[_0-9a-zA-Z]*}}F
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_DICT]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
    // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
    // CHECK:   destroy_value [[SELF_COPY]]
    // CHECK:   return [[RESULT]] : $()
  }
  // CHECK: } // end sil function '$s24objc_dictionary_bridging3FooC23bridge_Dictionary_param{{[_0-9a-zA-Z]*}}FTo'

  // Bridging dictionary results
  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC24bridge_Dictionary_result{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary
  @objc func bridge_Dictionary_result() -> Dictionary<Foo, Foo> { 
    // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $Foo):
    // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
    // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @$s24objc_dictionary_bridging3FooC24bridge_Dictionary_result{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
    // CHECK:   [[DICT:%[0-9]+]] = apply [[SWIFT_FN]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
    // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
    // CHECK:   destroy_value [[SELF_COPY]]

    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSD10FoundationE19_bridgeToObjectiveCSo12NSDictionaryCyF
    // CHECK:   [[BORROWED_DICT:%.*]] = begin_borrow [[DICT]]
    // CHECK:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[BORROWED_DICT]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
    // CHECK:   end_borrow [[BORROWED_DICT]]
    // CHECK:   destroy_value [[DICT]]
    // CHECK:   return [[NSDICT]] : $NSDictionary
  }
  // CHECK: } // end sil function '$s24objc_dictionary_bridging3FooC24bridge_Dictionary_result{{[_0-9a-zA-Z]*}}FTo'

  @objc var property: Dictionary<Foo, Foo> = [:]

  // Property getter
  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC8propertySDyA2CGvgTo : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary
  //                                 @$s24objc_dictionary_bridging3FooC8propertySDyA2CSo8NSObjectCSH10Foundationg_Gvpfi
  // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $Foo):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[GETTER:%[0-9]+]] = function_ref @$s24objc_dictionary_bridging3FooC8propertySDyA2CGvg : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  // CHECK:   [[DICT:%[0-9]+]] = apply [[GETTER]]([[BORROWED_SELF_COPY]]) : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSD10FoundationE19_bridgeToObjectiveCSo12NSDictionaryCyF
  // CHECK:   [[BORROWED_DICT:%.*]] = begin_borrow [[DICT]]
  // CHECK:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[BORROWED_DICT]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
  // CHECK:   end_borrow [[BORROWED_DICT]]
  // CHECK:   destroy_value [[DICT]]
  // CHECK:   return [[NSDICT]] : $NSDictionary
  // CHECK: } // end sil function

  // Property setter
  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC8propertySDyA2CGvsTo : $@convention(objc_method) (NSDictionary, Foo) -> ()
  // CHECK: bb0([[NSDICT:%[0-9]+]] : @unowned $NSDictionary, [[SELF:%[0-9]+]] : @unowned $Foo):
  // CHECK:   [[NSDICT_COPY:%.*]] = copy_value [[NSDICT]]
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @$sSD10FoundationE36_unconditionallyBridgeFromObjectiveCySDyxq_GSo12NSDictionaryCSgFZ
  // CHECK:   [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.some!enumelt, [[NSDICT_COPY]] : $NSDictionary
  // CHECK:   [[DICT_META:%[0-9]+]] = metatype $@thin Dictionary<Foo, Foo>.Type
  // CHECK:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]], [[DICT_META]])

  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[SETTER:%[0-9]+]] = function_ref @$s24objc_dictionary_bridging3FooC8propertySDyA2CGvs : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SETTER]]([[DICT]], [[BORROWED_SELF_COPY]]) : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   return [[RESULT]] : $()

  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC19nonVerbatimProperty{{[_0-9a-zA-Z]*}}vgTo : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary

  // CHECK-LABEL: sil private [thunk] [ossa] @$s24objc_dictionary_bridging3FooC19nonVerbatimProperty{{[_0-9a-zA-Z]*}}vsTo : $@convention(objc_method) (NSDictionary, Foo) -> ()
  @objc var nonVerbatimProperty: Dictionary<String, Int> = [:]
}

func ==(x: Foo, y: Foo) -> Bool { }

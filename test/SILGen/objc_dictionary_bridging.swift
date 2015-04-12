// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

@objc class Foo : NSObject {
  // Bridging dictionary parameters
  // CHECK-LABEL: sil hidden @_TToFC24objc_dictionary_bridging3Foo23bridge_Dictionary_param{{.*}} : $@cc(objc_method) @thin (NSDictionary, Foo) -> ()
  func bridge_Dictionary_param(dict: Dictionary<Foo, Foo>) {
    // CHECK: bb0([[NSDICT:%[0-9]+]] : $NSDictionary, [[SELF:%[0-9]+]] : $Foo):
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionary{{.*}} : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
    // CHECK-NEXT: [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.Some!enumelt.1, [[NSDICT]] : $NSDictionary
    // CHECK-NEXT:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]]) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>

    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foo23bridge_Dictionary_paramfS0_FGVSs10DictionaryS0_S0__T_ : $@cc(method) @thin (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[DICT]], [[SELF]]) : $@cc(method) @thin (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
    // CHECK:   return [[RESULT]] : $()
  }

  // Bridging dictionary results
  // CHECK-LABEL: sil hidden @_TToFC24objc_dictionary_bridging3Foo24bridge_Dictionary_result{{.*}} : $@cc(objc_method) @thin (Foo) -> @autoreleased NSDictionary
  func bridge_Dictionary_result() -> Dictionary<Foo, Foo> { 
    // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foo24bridge_Dictionary_result{{.*}} : $@cc(method) @thin (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
    // CHECK-NEXT:   [[DICT:%[0-9]+]] = apply [[SWIFT_FN]]([[SELF]]) : $@cc(method) @thin (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>

    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertDictionaryToNSDictionary{{.*}} : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
    // CHECK-NEXT:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[DICT]]) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
    // CHECK-NEXT:   autorelease_return [[NSDICT]] : $NSDictionary
  }

  var property: Dictionary<Foo, Foo> = [:]

  // Property getter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foog8propertyGVSs10DictionaryS0_S0__ : $@cc(objc_method) @thin (Foo) -> @autoreleased NSDictionary
  // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
  // CHECK:   [[GETTER:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foog8propertyGVSs10DictionaryS0_S0__ : $@cc(method) @thin (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  // CHECK:   [[DICT:%[0-9]+]] = apply [[GETTER]]([[SELF]]) : $@cc(method) @thin (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertDictionaryToNSDictionary{{.*}} : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
  // CHECK:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[DICT]]) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
  // CHECK:   autorelease_return [[NSDICT]] : $NSDictionary

  // Property setter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foos8propertyGVSs10DictionaryS0_S0__ : $@cc(objc_method) @thin (NSDictionary, Foo) -> ()
  // CHECK: bb0([[NSDICT:%[0-9]+]] : $NSDictionary, [[SELF:%[0-9]+]] : $Foo):
// CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionary{{.*}} : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
// CHECK: [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.Some!enumelt.1, [[NSDICT]] : $NSDictionary
// CHECK:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]]) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>

// CHECK:   [[SETTER:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foos8propertyGVSs10DictionaryS0_S0__ : $@cc(method) @thin (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
// CHECK:   [[RESULT:%[0-9]+]] = apply [[SETTER]]([[DICT]], [[SELF]]) : $@cc(method) @thin (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
// CHECK:   return [[RESULT]] : $()

  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foog19nonVerbatimProperty{{.*}} : $@cc(objc_method) @thin (Foo) -> @autoreleased NSDictionary

  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foos19nonVerbatimProperty{{.*}} : $@cc(objc_method) @thin (NSDictionary, Foo) -> ()
  @objc var nonVerbatimProperty: Dictionary<String, Int> = [:]
}

func ==(x: Foo, y: Foo) -> Bool { }

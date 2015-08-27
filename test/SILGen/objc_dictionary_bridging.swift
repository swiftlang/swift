// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

@objc class Foo : NSObject {
  // Bridging dictionary parameters
  // CHECK-LABEL: sil hidden @_TToFC24objc_dictionary_bridging3Foo23bridge_Dictionary_param{{.*}} : $@convention(objc_method) (NSDictionary, Foo) -> ()
  func bridge_Dictionary_param(dict: Dictionary<Foo, Foo>) {
    // CHECK: bb0([[NSDICT:%[0-9]+]] : $NSDictionary, [[SELF:%[0-9]+]] : $Foo):
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionary{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
    // CHECK-NEXT: [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.Some!enumelt.1, [[NSDICT]] : $NSDictionary
    // CHECK-NEXT:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>

    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foo23bridge_Dictionary_paramfS0_FGVSs10DictionaryS0_S0__T_ : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[DICT]], [[SELF]]) : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
    // CHECK:   return [[RESULT]] : $()
  }

  // Bridging dictionary results
  // CHECK-LABEL: sil hidden @_TToFC24objc_dictionary_bridging3Foo24bridge_Dictionary_result{{.*}} : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary
  func bridge_Dictionary_result() -> Dictionary<Foo, Foo> { 
    // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foo24bridge_Dictionary_result{{.*}} : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
    // CHECK-NEXT:   [[DICT:%[0-9]+]] = apply [[SWIFT_FN]]([[SELF]]) : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>

    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertDictionaryToNSDictionary{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
    // CHECK-NEXT:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
    // CHECK-NEXT:   autorelease_return [[NSDICT]] : $NSDictionary
  }

  var property: Dictionary<Foo, Foo> = [:]

  // Property getter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foog8propertyGVSs10DictionaryS0_S0__ : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary
  // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
  // CHECK:   [[GETTER:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foog8propertyGVSs10DictionaryS0_S0__ : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  // CHECK:   [[DICT:%[0-9]+]] = apply [[GETTER]]([[SELF]]) : $@convention(method) (@guaranteed Foo) -> @owned Dictionary<Foo, Foo>
  
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertDictionaryToNSDictionary{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
  // CHECK:   [[NSDICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (@owned Dictionary<τ_0_0, τ_0_1>) -> @owned NSDictionary
  // CHECK:   autorelease_return [[NSDICT]] : $NSDictionary

  // Property setter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foos8propertyGVSs10DictionaryS0_S0__ : $@convention(objc_method) (NSDictionary, Foo) -> ()
  // CHECK: bb0([[NSDICT:%[0-9]+]] : $NSDictionary, [[SELF:%[0-9]+]] : $Foo):
// CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionary{{.*}} : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
// CHECK: [[OPT_NSDICT:%[0-9]+]] = enum $Optional<NSDictionary>, #Optional.Some!enumelt.1, [[NSDICT]] : $NSDictionary
// CHECK:   [[DICT:%[0-9]+]] = apply [[CONVERTER]]<Foo, Foo>([[OPT_NSDICT]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>

// CHECK:   [[SETTER:%[0-9]+]] = function_ref @_TFC24objc_dictionary_bridging3Foos8propertyGVSs10DictionaryS0_S0__ : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
// CHECK:   [[RESULT:%[0-9]+]] = apply [[SETTER]]([[DICT]], [[SELF]]) : $@convention(method) (@owned Dictionary<Foo, Foo>, @guaranteed Foo) -> ()
// CHECK:   return [[RESULT]] : $()

  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foog19nonVerbatimProperty{{.*}} : $@convention(objc_method) (Foo) -> @autoreleased NSDictionary

  // CHECK-LABEL: sil hidden [transparent] @_TToFC24objc_dictionary_bridging3Foos19nonVerbatimProperty{{.*}} : $@convention(objc_method) (NSDictionary, Foo) -> ()
  @objc var nonVerbatimProperty: Dictionary<String, Int> = [:]
}

func ==(x: Foo, y: Foo) -> Bool { }

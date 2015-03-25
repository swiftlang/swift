// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import gizmo

@objc class Foo : NSObject {
  // Bridging set parameters
  // CHECK-LABEL: sil hidden @_TToFC17objc_set_bridging3Foo16bridge_Set_param{{.*}} : $@cc(objc_method) @thin (NSSet, Foo) -> ()
  func bridge_Set_param(s: Set<Foo>) {
    // CHECK: bb0([[NSSET:%[0-9]+]] : $NSSet, [[SELF:%[0-9]+]] : $Foo):
    // CHECK:   strong_retain [[NSSET]] : $NSSet
    // CHECK:   strong_retain [[SELF]] : $Foo
    // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation18_convertNSSetToSet{{.*}} : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
    // CHECK: [[OPT_NSSET:%[0-9]+]] = enum $Optional<NSSet>, #Optional.Some!enumelt.1, [[NSSET]] : $NSSet
    // CHECK:   [[SET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[OPT_NSSET]]) : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
    // CHECK:   [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC17objc_set_bridging3Foo16bridge_Set_param{{.*}} : $@cc(method) @thin (@owned Set<Foo>, @owned Foo) -> ()
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[SWIFT_FN]]([[SET]], [[SELF]]) : $@cc(method) @thin (@owned Set<Foo>, @owned Foo) -> ()
    // CHECK:   return [[RESULT]] : $()
  }

  // Bridging set results
  // CHECK-LABEL: sil hidden @_TToFC17objc_set_bridging3Foo17bridge_Set_result{{.*}} : $@cc(objc_method) @thin (Foo) -> @autoreleased NSSet {
  func bridge_Set_result() -> Set<Foo> { 
    // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
    // CHECK: strong_retain [[SELF]] : $Foo
    // CHECK: [[SWIFT_FN:%[0-9]+]] = function_ref @_TFC17objc_set_bridging3Foo17bridge_Set_result{{.*}} : $@cc(method) @thin (@owned Foo) -> @owned Set<Foo>
    // CHECK: [[SET:%[0-9]+]] = apply [[SWIFT_FN]]([[SELF]]) : $@cc(method) @thin (@owned Foo) -> @owned Set<Foo>
    // CHECK: [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation18_convertSetToNSSet{{.*}} : $@thin <τ_0_0 where τ_0_0 : Hashable> (@owned Set<τ_0_0>) -> @owned NSSet
    // CHECK: [[NSSET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[SET]]) : $@thin <τ_0_0 where τ_0_0 : Hashable> (@owned Set<τ_0_0>) -> @owned NSSet
    // CHECK: autorelease_return [[NSSET]] : $NSSet
  }

  var property: Set<Foo> = Set()

  // Property getter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC17objc_set_bridging3Foog8property{{.*}} : $@cc(objc_method) @thin (Foo) -> @autoreleased NSSet
  // CHECK: bb0([[SELF:%[0-9]+]] : $Foo):
  // CHECK:   strong_retain [[SELF]] : $Foo
  // CHECK:   [[GETTER:%[0-9]+]] = function_ref @_TFC17objc_set_bridging3Foog8property{{.*}} : $@cc(method) @thin (@owned Foo) -> @owned Set<Foo>
  // CHECK:   [[SET:%[0-9]+]] = apply [[GETTER]]([[SELF]]) : $@cc(method) @thin (@owned Foo) -> @owned Set<Foo>
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation18_convertSetToNSSet{{.*}} : $@thin <τ_0_0 where τ_0_0 : Hashable> (@owned Set<τ_0_0>) -> @owned NSSet
  // CHECK:   [[NSSET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[SET]]) : $@thin <τ_0_0 where τ_0_0 : Hashable> (@owned Set<τ_0_0>) -> @owned NSSet
  // CHECK:   autorelease_return [[NSSET]] : $NSSet
  
  // Property setter
  // CHECK-LABEL: sil hidden [transparent] @_TToFC17objc_set_bridging3Foos8property{{.*}} : $@cc(objc_method) @thin (NSSet, Foo) -> () {
  // CHECK: bb0([[NSSET:%[0-9]+]] : $NSSet, [[SELF:%[0-9]+]] : $Foo):
  // CHECK:   strong_retain [[NSSET]] : $NSSet
  // CHECK:   strong_retain [[SELF]] : $Foo
  // CHECK:   [[CONVERTER:%[0-9]+]] = function_ref @_TF10Foundation18_convertNSSetToSet{{.*}} : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK: [[OPT_NSSET:%[0-9]+]] = enum $Optional<NSSet>, #Optional.Some!enumelt.1, [[NSSET]] : $NSSet
  // CHECK:   [[SET:%[0-9]+]] = apply [[CONVERTER]]<Foo>([[OPT_NSSET]]) : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK:   [[SETTER:%[0-9]+]] = function_ref @_TFC17objc_set_bridging3Foos8property{{.*}} : $@cc(method) @thin (@owned Set<Foo>, @owned Foo) -> ()
  // CHECK:   [[RESULT:%[0-9]+]] = apply [[SETTER]]([[SET]], [[SELF]]) : $@cc(method) @thin (@owned Set<Foo>, @owned Foo) -> ()
  // CHECK:   return [[RESULT]] : $()
  
  // CHECK-LABEL: sil hidden [transparent] @_TToFC17objc_set_bridging3Foog19nonVerbatimProperty{{.*}} : $@cc(objc_method) @thin (Foo) -> @autoreleased NSSet
  // CHECK-LABEL: sil hidden [transparent] @_TToFC17objc_set_bridging3Foos19nonVerbatimProperty{{.*}} : $@cc(objc_method) @thin (NSSet, Foo) -> () {
  @objc var nonVerbatimProperty: Set<String> = Set()
}

func ==(x: Foo, y: Foo) -> Bool { }

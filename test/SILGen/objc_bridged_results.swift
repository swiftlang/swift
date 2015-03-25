// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -import-objc-header %S/Inputs/objc_bridged_results.h | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results11testNonnullFCSo4TestGSaPSs9AnyObject__
func testNonnull(obj: Test) -> [AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullArray!getter.1.foreign : Test -> () -> [AnyObject] , $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_VAL]]) : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<AnyObject>
  return obj.nonnullArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results12testNullableFCSo4TestGSqGSaPSs9AnyObject___
func testNullable(obj: Test) -> [AnyObject]? {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullableArray!getter.1.foreign : Test -> () -> [AnyObject]? , $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[RESULT_BOX:%[0-9]+]] = alloc_stack $Optional<Array<AnyObject>>
  // CHECK: [[COCOA_BOX:%[0-9]+]] = alloc_stack $Optional<NSArray>
  // CHECK: store [[COCOA_VAL]] to [[COCOA_BOX]]#1 : $*Optional<NSArray>
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum_addr [[COCOA_BOX]]#1 : $*Optional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_BOX_NON_NIL:%[0-9]+]] = unchecked_take_enum_data_addr [[COCOA_BOX]]#1 : $*Optional<NSArray>, #Optional.Some!enumelt.1
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = load [[COCOA_BOX_NON_NIL]] : $*NSArray
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_SOME_VAL]]) : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT_SOME_BOX:%[0-9]+]] = init_enum_data_addr [[RESULT_BOX]]#1 : $*Optional<Array<AnyObject>>, #Optional.Some!enumelt.1
  // CHECK: store [[RESULT_VAL]] to [[RESULT_SOME_BOX]] : $*Array<AnyObject>
  // CHECK: inject_enum_addr [[RESULT_BOX]]#1 : $*Optional<Array<AnyObject>>, #Optional.Some!enumelt.1
  // CHECK: br [[FINISH:[^, ]+]]
  
  // CHECK: [[CASE_NIL]]:
  // CHECK: inject_enum_addr [[RESULT_BOX]]#1 : $*Optional<Array<AnyObject>>, #Optional.None!enumelt
  // CHECK: br [[FINISH]]
  
  // CHECK: [[FINISH]]:
  // CHECK: [[RESULT:%[0-9]+]] = load [[RESULT_BOX]]#1 : $*Optional<Array<AnyObject>>
  // CHECK: dealloc_stack [[COCOA_BOX]]#0 : $*@local_storage Optional<NSArray>
  // CHECK: dealloc_stack [[RESULT_BOX]]#0 : $*@local_storage Optional<Array<AnyObject>>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Optional<Array<AnyObject>>
  return obj.nullableArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results19testNullUnspecifiedFCSo4TestGSQGSaPSs9AnyObject___
func testNullUnspecified(obj: Test) -> [AnyObject]! {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullUnspecifiedArray!getter.1.foreign : Test -> () -> [AnyObject]! , $@cc(objc_method) @thin (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[RESULT_BOX:%[0-9]+]] = alloc_stack $ImplicitlyUnwrappedOptional<Array<AnyObject>>
  // CHECK: [[COCOA_BOX:%[0-9]+]] = alloc_stack $ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: store [[COCOA_VAL]] to [[COCOA_BOX]]#1 : $*ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum_addr [[COCOA_BOX]]#1 : $*ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_BOX_NON_NIL:%[0-9]+]] = unchecked_take_enum_data_addr [[COCOA_BOX]]#1 : $*ImplicitlyUnwrappedOptional<NSArray>, #ImplicitlyUnwrappedOptional.Some!enumelt.1
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = load [[COCOA_BOX_NON_NIL]] : $*NSArray
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_SOME_VAL]]) : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT_SOME_BOX:%[0-9]+]] = init_enum_data_addr [[RESULT_BOX]]#1 : $*ImplicitlyUnwrappedOptional<Array<AnyObject>>, #ImplicitlyUnwrappedOptional.Some!enumelt.1
  // CHECK: store [[RESULT_VAL]] to [[RESULT_SOME_BOX]] : $*Array<AnyObject>
  // CHECK: inject_enum_addr [[RESULT_BOX]]#1 : $*ImplicitlyUnwrappedOptional<Array<AnyObject>>, #ImplicitlyUnwrappedOptional.Some!enumelt.1
  // CHECK: br [[FINISH:[^, ]+]]
  
  // CHECK: [[CASE_NIL]]:
  // CHECK: inject_enum_addr [[RESULT_BOX]]#1 : $*ImplicitlyUnwrappedOptional<Array<AnyObject>>, #ImplicitlyUnwrappedOptional.None!enumelt
  // CHECK: br [[FINISH]]
  
  // CHECK: [[FINISH]]:
  // CHECK: [[RESULT:%[0-9]+]] = load [[RESULT_BOX]]#1 : $*ImplicitlyUnwrappedOptional<Array<AnyObject>>
  // CHECK: dealloc_stack [[COCOA_BOX]]#0 : $*@local_storage ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: dealloc_stack [[RESULT_BOX]]#0 : $*@local_storage ImplicitlyUnwrappedOptional<Array<AnyObject>>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $ImplicitlyUnwrappedOptional<Array<AnyObject>>
  return obj.nullUnspecifiedArray
} // CHECK: {{^}$}}


// CHECK-LABEL: sil hidden @_TF20objc_bridged_results21testNonnullDictionaryFCSo4TestGVSs10DictionaryCSo8NSObjectPSs9AnyObject__
func testNonnullDictionary(obj: Test) -> [NSObject: AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullDictionary!getter.1.foreign : Test -> () -> [NSObject : AnyObject] , $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionaryUSs8Hashable_Ss9AnyObject__FGSqCSo12NSDictionary_GVSs10DictionaryQ_Q0__ : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<NSObject, AnyObject>([[COCOA_VAL]]) : $@thin <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Dictionary<NSObject, AnyObject>
  return obj.nonnullDictionary
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results14testNonnullSetFCSo4TestGVSs3SetCSo8NSObject_
func testNonnullSet(obj: Test) -> Set<NSObject> {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullSet!getter.1.foreign : Test -> () -> Set<NSObject> , $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSSet>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation18_convertNSSetToSetUSs8Hashable__FGSqCSo5NSSet_GVSs3SetQ__ : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<NSObject>([[COCOA_VAL]]) : $@thin <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Set<NSObject>
  return obj.nonnullSet
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results17testNonnullStringFCSo4TestSS
func testNonnullString(obj: Test) -> String {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullString!getter.1.foreign : Test -> () -> String , $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@cc(objc_method) @thin (Test) -> @autoreleased Optional<NSString>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @swift_NSStringToString : $@thin (@owned Optional<NSString>) -> @owned String
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]]) : $@thin (@owned Optional<NSString>) -> @owned String
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $String
  return obj.nonnullString
} // CHECK: {{^}$}}


// Note: This doesn't really "work" in that it doesn't accept a nil value the
// way the others do, because subscripts are thunked. But the main thing is
// not to crash trying to generate the thunk.
// CHECK-LABEL: sil hidden @_TF20objc_bridged_results20testNonnullSubscriptFCSo4TestGSaPSs9AnyObject__
func testNonnullSubscript(obj: Test) -> [AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.subscript!getter.1.foreign : Test -> (Int) -> [AnyObject] , $@cc(objc_method) @thin (Int, Test) -> @autoreleased NSArray
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, %0) : $@cc(objc_method) @thin (Int, Test) -> @autoreleased NSArray
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[COCOA_VAL]] : $NSArray
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_SOME_VAL]]) : $@thin <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<AnyObject>
  return obj[0]
} // CHECK: {{^}$}}

// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -import-objc-header %S/Inputs/objc_bridged_results.h | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results11testNonnullFCSo4TestGSaPSs9AnyObject__
func testNonnull(obj: Test) -> [AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullArray!getter.1.foreign : Test -> () -> [AnyObject] , $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_VAL]]) : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<AnyObject>
  return obj.nonnullArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results12testNullableFCSo4TestGSqGSaPSs9AnyObject___
func testNullable(obj: Test) -> [AnyObject]? {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullableArray!getter.1.foreign : Test -> () -> [AnyObject]? , $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum [[COCOA_VAL]] : $Optional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = unchecked_enum_data [[COCOA_VAL]] : $Optional<NSArray>, #Optional.Some!enumelt.1
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_SOME_VAL]]) : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $Optional<Array<AnyObject>>, #Optional.Some!enumelt.1, [[RESULT_VAL]] : $Array<AnyObject>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $Optional<Array<AnyObject>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $Optional<Array<AnyObject>>, #Optional.None!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $Optional<Array<AnyObject>>)
  
  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : $Optional<Array<AnyObject>>):
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Optional<Array<AnyObject>>
  return obj.nullableArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results19testNullUnspecifiedFCSo4TestGSQGSaPSs9AnyObject___
func testNullUnspecified(obj: Test) -> [AnyObject]! {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullUnspecifiedArray!getter.1.foreign : Test -> () -> [AnyObject]! , $@convention(objc_method) (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum [[COCOA_VAL]] : $ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = unchecked_enum_data [[COCOA_VAL]] : $ImplicitlyUnwrappedOptional<NSArray>, #ImplicitlyUnwrappedOptional.Some!enumelt.1
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.Some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_SOME_VAL]]) : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $ImplicitlyUnwrappedOptional<Array<AnyObject>>, #ImplicitlyUnwrappedOptional.Some!enumelt.1, [[RESULT_VAL]] : $Array<AnyObject>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $ImplicitlyUnwrappedOptional<Array<AnyObject>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $ImplicitlyUnwrappedOptional<Array<AnyObject>>, #ImplicitlyUnwrappedOptional.None!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $ImplicitlyUnwrappedOptional<Array<AnyObject>>)

  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : $ImplicitlyUnwrappedOptional<Array<AnyObject>>):
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $ImplicitlyUnwrappedOptional<Array<AnyObject>>
  return obj.nullUnspecifiedArray
} // CHECK: {{^}$}}


// CHECK-LABEL: sil hidden @_TF20objc_bridged_results21testNonnullDictionaryFCSo4TestGVSs10DictionaryCSo8NSObjectPSs9AnyObject__
func testNonnullDictionary(obj: Test) -> [NSObject: AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullDictionary!getter.1.foreign : Test -> () -> [NSObject : AnyObject] , $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation32_convertNSDictionaryToDictionaryUSs8Hashable_Ss9AnyObject__FGSqCSo12NSDictionary_GVSs10DictionaryQ_Q0__ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<NSObject, AnyObject>([[COCOA_VAL]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : NSObject, τ_0_0 : Hashable, τ_0_1 : AnyObject> (@owned Optional<NSDictionary>) -> @owned Dictionary<τ_0_0, τ_0_1>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Dictionary<NSObject, AnyObject>
  return obj.nonnullDictionary
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results14testNonnullSetFCSo4TestGVSs3SetCSo8NSObject_
func testNonnullSet(obj: Test) -> Set<NSObject> {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullSet!getter.1.foreign : Test -> () -> Set<NSObject> , $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation18_convertNSSetToSetUSs8Hashable__FGSqCSo5NSSet_GVSs3SetQ__ : $@convention(thin) <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<NSObject>([[COCOA_VAL]]) : $@convention(thin) <τ_0_0 where τ_0_0 : NSObject, τ_0_0 : Hashable> (@owned Optional<NSSet>) -> @owned Set<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Set<NSObject>
  return obj.nonnullSet
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results17testNonnullStringFCSo4TestSS
func testNonnullString(obj: Test) -> String {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullString!getter.1.foreign : Test -> () -> String , $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @swift_NSStringToString : $@convention(thin) (@owned Optional<NSString>) -> @owned String
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]]) : $@convention(thin) (@owned Optional<NSString>) -> @owned String
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $String
  return obj.nonnullString
} // CHECK: {{^}$}}


// Note: This doesn't really "work" in that it doesn't accept a nil value the
// way the others do, because subscripts are thunked. But the main thing is
// not to crash trying to generate the thunk.
// CHECK-LABEL: sil hidden @_TF20objc_bridged_results20testNonnullSubscriptFCSo4TestGSaPSs9AnyObject__
func testNonnullSubscript(obj: Test) -> [AnyObject] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.subscript!getter.1.foreign : Test -> (Int) -> [AnyObject] , $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, %0) : $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: strong_retain_autoreleased [[COCOA_VAL]]
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TF10Foundation22_convertNSArrayToArrayU__FGSqCSo7NSArray_GSaQ__ : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyObject>([[COCOA_VAL]]) : $@convention(thin) <τ_0_0> (@owned Optional<NSArray>) -> @owned Array<τ_0_0>
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<AnyObject>
  return obj[0]
} // CHECK: {{^}$}}

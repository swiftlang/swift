// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s -import-objc-header %S/Inputs/objc_bridged_results.h | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results11testNonnullFCSo4TestGSaP__
func testNonnull(_ obj: Test) -> [Any] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullArray!getter.1.foreign : (Test) -> () -> [Any] , $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSa36_unconditionallyBridgeFromObjectiveCfGSqCSo7NSArray_GSax_
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_VAL]], [[ARRAY_META]])
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<Any>
  return obj.nonnullArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results12testNullableFCSo4TestGSqGSaP___
func testNullable(_ obj: Test) -> [Any]? {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullableArray!getter.1.foreign : (Test) -> () -> [Any]? , $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum [[COCOA_VAL]] : $Optional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = unchecked_enum_data [[COCOA_VAL]] : $Optional<NSArray>, #Optional.some!enumelt.1
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSa36_unconditionallyBridgeFromObjectiveCfGSqCSo7NSArray_GSax_
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_SOME_VAL]], [[ARRAY_META]])
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.some!enumelt.1, [[RESULT_VAL]] : $Array<Any>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $Optional<Array<Any>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.none!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $Optional<Array<Any>>)
  
  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : $Optional<Array<Any>>):
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Optional<Array<Any>>
  return obj.nullableArray
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results19testNullUnspecifiedFCSo4TestGSQGSaP___
func testNullUnspecified(_ obj: Test) -> [Any]! {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nullUnspecifiedArray!getter.1.foreign : (Test) -> () -> [Any]! , $@convention(objc_method) (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: [[IS_NON_NIL:%[0-9]+]] = select_enum [[COCOA_VAL]] : $ImplicitlyUnwrappedOptional<NSArray>
  // CHECK: cond_br [[IS_NON_NIL]], [[CASE_NON_NIL:[^, ]+]], [[CASE_NIL:[^, ]+]]

  // CHECK: [[CASE_NON_NIL]]:
  // CHECK: [[COCOA_VAL_NON_NIL:%[0-9]+]] = unchecked_enum_data [[COCOA_VAL]] : $ImplicitlyUnwrappedOptional<NSArray>, #ImplicitlyUnwrappedOptional.some!enumelt.1
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSa36_unconditionallyBridgeFromObjectiveCfGSqCSo7NSArray_GSax_
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_SOME_VAL]], [[ARRAY_META]])
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $ImplicitlyUnwrappedOptional<Array<Any>>, #ImplicitlyUnwrappedOptional.some!enumelt.1, [[RESULT_VAL]] : $Array<Any>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $ImplicitlyUnwrappedOptional<Array<Any>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $ImplicitlyUnwrappedOptional<Array<Any>>, #ImplicitlyUnwrappedOptional.none!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $ImplicitlyUnwrappedOptional<Array<Any>>)

  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : $ImplicitlyUnwrappedOptional<Array<Any>>):
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $ImplicitlyUnwrappedOptional<Array<Any>>
  return obj.nullUnspecifiedArray
} // CHECK: {{^}$}}


// CHECK-LABEL: sil hidden @_TF20objc_bridged_results21testNonnullDictionaryFCSo4TestGVs10DictionaryVs11AnyHashableP__
func testNonnullDictionary(_ obj: Test) -> [AnyHashable: Any] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullDictionary!getter.1.foreign : (Test) -> () -> [AnyHashable : Any] , $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationVs10Dictionary36_unconditionallyBridgeFromObjectiveCfGSqCSo12NSDictionary_GS0_xq__
  // CHECK: [[DICT_META:%[0-9]+]] = metatype $@thin Dictionary<AnyHashable, Any>.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyHashable, Any>([[COCOA_VAL]], [[DICT_META]])
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Dictionary<AnyHashable, Any>
  return obj.nonnullDictionary
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results14testNonnullSetFCSo4TestGVs3SetVs11AnyHashable_
func testNonnullSet(_ obj: Test) -> Set<AnyHashable> {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullSet!getter.1.foreign : (Test) -> () -> Set<AnyHashable> , $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationVs3Set36_unconditionallyBridgeFromObjectiveCfGSqCSo5NSSet_GS0_x_
  // CHECK: [[SET_META:%[0-9]+]] = metatype $@thin Set<AnyHashable>.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyHashable>([[COCOA_VAL]], [[SET_META]])
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Set<AnyHashable>
  return obj.nonnullSet
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results17testNonnullStringFCSo4TestSS
func testNonnullString(_ obj: Test) -> String {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.nonnullString!getter.1.foreign : (Test) -> () -> String , $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]](%0) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSS36_unconditionallyBridgeFromObjectiveCfGSqCSo8NSString_SS
  // CHECK: [[STRING_META:%[0-9]+]] = metatype $@thin String.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]], [[STRING_META]]) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $String
  return obj.nonnullString
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF20objc_bridged_results13testClassPropFT_SS
func testClassProp() -> String {
  // CHECK: [[CLASS:%.+]] = metatype $@thick Test.Type
  // CHECK: [[METHOD:%.+]] = class_method [volatile] [[CLASS]] : $@thick Test.Type, #Test.nonnullSharedString!getter.1.foreign : (Test.Type) -> () -> String , $@convention(objc_method) (@objc_metatype Test.Type) -> @autoreleased Optional<NSString>
  // CHECK: [[OBJC_CLASS:%.+]] = thick_to_objc_metatype [[CLASS]] : $@thick Test.Type to $@objc_metatype Test.Type
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[OBJC_CLASS]]) : $@convention(objc_method) (@objc_metatype Test.Type) -> @autoreleased Optional<NSString>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSS36_unconditionallyBridgeFromObjectiveCfGSqCSo8NSString_SS
  // CHECK: [[STRING_META:%[0-9]+]] = metatype $@thin String.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]], [[STRING_META]]) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: return [[RESULT]] : $String
  return Test.nonnullSharedString
} // CHECK: {{^}$}}


// Note: This doesn't really "work" in that it doesn't accept a nil value the
// way the others do, because subscripts are thunked. But the main thing is
// not to crash trying to generate the thunk.
// CHECK-LABEL: sil hidden @_TF20objc_bridged_results20testNonnullSubscriptFCSo4TestGSaP__
func testNonnullSubscript(_ obj: Test) -> [Any] {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $Test, #Test.subscript!getter.1.foreign : (Test) -> (Int) -> [Any] , $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, %0) : $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @_TZFE10FoundationSa36_unconditionallyBridgeFromObjectiveCfGSqCSo7NSArray_GSax_
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type,
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_VAL]], [[ARRAY_META]])
  // CHECK: strong_release %0 : $Test
  // CHECK: return [[RESULT]] : $Array<Any>
  return obj[0]
} // CHECK: {{^}$}}


// CHECK-LABEL: sil hidden @_TF20objc_bridged_results19testPerformSelectorFCSo8NSObjectT_
func testPerformSelector(_ obj: NSObject) {
  // CHECK: [[METHOD:%[0-9]+]] = class_method [volatile] %0 : $NSObject, #NSObject.perform!1.foreign
  // CHECK: [[RESULT:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, {{%[0-9]+}}, %0)
  _ = obj.perform("foo", with: nil)
  // CHECK-NOT: {{(retain|release).+}}[[RESULT]]
} // CHECK: {{^}$}}

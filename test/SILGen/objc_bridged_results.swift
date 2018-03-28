
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -module-name objc_bridged_results -emit-silgen %s -Xllvm -sil-print-debuginfo -import-objc-header %S/Inputs/objc_bridged_results.h -enable-sil-ownership | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @$S20objc_bridged_results11testNonnullySayypGSo4TestCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
// CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nonnullArray!getter.1.foreign : (Test) -> () -> [Any], $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
// CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
// CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSa10FoundationE36_unconditionallyBridgeFromObjectiveCySayxGSo7NSArrayCSgFZ
// CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
// CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_VAL]], [[ARRAY_META]])
// CHECK-NOT: destroy_value %0 : $Test
// CHECK: return [[RESULT]] : $Array<Any>
func testNonnull(_ obj: Test) -> [Any] {
  return obj.nonnullArray
} // CHECK: } // end sil function '$S20objc_bridged_results11testNonnullySayypGSo4TestCF'

// CHECK-LABEL: sil hidden @$S20objc_bridged_results12testNullableySayypGSgSo4TestCF
func testNullable(_ obj: Test) -> [Any]? {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nullableArray!getter.1.foreign : (Test) -> () -> [Any]?, $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: switch_enum [[COCOA_VAL]] : $Optional<NSArray>, case #Optional.some!enumelt.1: [[CASE_NON_NIL:bb[0-9]+]], case #Optional.none!enumelt: [[CASE_NIL:bb[0-9]+]]
  //
  // CHECK: [[CASE_NON_NIL]]([[COCOA_VAL_NON_NIL:%.*]] : @owned $NSArray):
  // CHECK-NOT: unchecked_enum_data
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSa10FoundationE36_unconditionallyBridgeFromObjectiveCySayxGSo7NSArrayCSgFZ
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_SOME_VAL]], [[ARRAY_META]])
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.some!enumelt.1, [[RESULT_VAL]] : $Array<Any>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $Optional<Array<Any>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.none!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $Optional<Array<Any>>)
  
  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : @owned $Optional<Array<Any>>):
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $Optional<Array<Any>>
  return obj.nullableArray
} // CHECK: } // end sil function '$S20objc_bridged_results12testNullableySayypGSgSo4TestCF'

// CHECK-LABEL: sil hidden @$S20objc_bridged_results19testNullUnspecifiedySayypGSgSo4TestCF
func testNullUnspecified(_ obj: Test) -> [Any]! {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nullUnspecifiedArray!getter.1.foreign : (Test) -> () -> [Any]?, $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSArray>
  // CHECK: switch_enum [[COCOA_VAL]] : $Optional<NSArray>, case #Optional.some!enumelt.1: [[CASE_NON_NIL:bb[0-9]+]], case #Optional.none!enumelt: [[CASE_NIL:bb[0-9]+]]

  // CHECK: [[CASE_NON_NIL]]([[COCOA_VAL_NON_NIL:%.*]] : @owned $NSArray):
  // CHECK-NOT: unchecked_enum_data
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSa10FoundationE36_unconditionallyBridgeFromObjectiveCySayxGSo7NSArrayCSgFZ
  // CHECK: [[COCOA_SOME_VAL:%[0-9]+]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[COCOA_VAL_NON_NIL]]
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type
  // CHECK: [[RESULT_VAL:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_SOME_VAL]], [[ARRAY_META]])
  // CHECK: [[RESULT_SOME:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.some!enumelt.1, [[RESULT_VAL]] : $Array<Any>
  // CHECK: br [[FINISH:bb[0-9]+]]([[RESULT_SOME]] : $Optional<Array<Any>>)
  
  // CHECK: [[CASE_NIL]]:
  // CHECK:   [[RESULT_NONE:%[0-9]+]] = enum $Optional<Array<Any>>, #Optional.none!enumelt
  // CHECK: br [[FINISH]]([[RESULT_NONE]] : $Optional<Array<Any>>)

  // CHECK: [[FINISH]]([[RESULT:%[0-9]+]] : @owned $Optional<Array<Any>>):
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $Optional<Array<Any>>
  return obj.nullUnspecifiedArray
} // CHECK: } // end sil function '$S20objc_bridged_results19testNullUnspecifiedySayypGSgSo4TestCF'


// CHECK-LABEL: sil hidden @$S20objc_bridged_results21testNonnullDictionaryys0F0Vys11AnyHashableVypGSo4TestCF
func testNonnullDictionary(_ obj: Test) -> [AnyHashable: Any] {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nonnullDictionary!getter.1.foreign : (Test) -> () -> [AnyHashable : Any], $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSDictionary>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$Ss10DictionaryV10FoundationE36_unconditionallyBridgeFromObjectiveCyAByxq_GSo12NSDictionaryCSgFZ
  // CHECK: [[DICT_META:%[0-9]+]] = metatype $@thin Dictionary<AnyHashable, Any>.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyHashable, Any>([[COCOA_VAL]], [[DICT_META]])
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $Dictionary<AnyHashable, Any>
  return obj.nonnullDictionary
} // CHECK: } // end sil function '$S20objc_bridged_results21testNonnullDictionaryys0F0Vys11AnyHashableVypGSo4TestCF'

// CHECK-LABEL: sil hidden @$S20objc_bridged_results14testNonnullSetys0F0Vys11AnyHashableVGSo4TestCF
func testNonnullSet(_ obj: Test) -> Set<AnyHashable> {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nonnullSet!getter.1.foreign : (Test) -> () -> Set<AnyHashable>, $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSSet>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$Ss3SetV10FoundationE36_unconditionallyBridgeFromObjectiveCyAByxGSo5NSSetCSgFZ
  // CHECK: [[SET_META:%[0-9]+]] = metatype $@thin Set<AnyHashable>.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<AnyHashable>([[COCOA_VAL]], [[SET_META]])
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $Set<AnyHashable>
  return obj.nonnullSet
} // CHECK: } // end sil function '$S20objc_bridged_results14testNonnullSetys0F0Vys11AnyHashableVGSo4TestCF'

// CHECK-LABEL: sil hidden @$S20objc_bridged_results17testNonnullStringySSSo4TestCF
func testNonnullString(_ obj: Test) -> String {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.nonnullString!getter.1.foreign : (Test) -> () -> String, $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[ARG]]) : $@convention(objc_method) (Test) -> @autoreleased Optional<NSString>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: [[STRING_META:%[0-9]+]] = metatype $@thin String.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]], [[STRING_META]]) : $@convention(method) (@guaranteed Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $String
  return obj.nonnullString
} // CHECK: } // end sil function '$S20objc_bridged_results17testNonnullStringySSSo4TestCF'

// CHECK-LABEL: sil hidden @$S20objc_bridged_results13testClassPropSSyF
func testClassProp() -> String {
  // CHECK: [[CLASS:%.+]] = metatype $@objc_metatype Test.Type
  // CHECK: [[METHOD:%.+]] = objc_method [[CLASS]] : $@objc_metatype Test.Type, #Test.nonnullSharedString!getter.1.foreign : (Test.Type) -> () -> String, $@convention(objc_method) (@objc_metatype Test.Type) -> @autoreleased Optional<NSString>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]([[CLASS]]) : $@convention(objc_method) (@objc_metatype Test.Type) -> @autoreleased Optional<NSString>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK: [[STRING_META:%[0-9]+]] = metatype $@thin String.Type
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]([[COCOA_VAL]], [[STRING_META]]) : $@convention(method) (@guaranteed Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: return [[RESULT]] : $String
  return Test.nonnullSharedString
} // CHECK: } // end sil function '$S20objc_bridged_results13testClassPropSSyF'


// Note: This doesn't really "work" in that it doesn't accept a nil value the
// way the others do, because subscripts are thunked. But the main thing is
// not to crash trying to generate the thunk.
// CHECK-LABEL: sil hidden @$S20objc_bridged_results20testNonnullSubscriptySayypGSo4TestCF
func testNonnullSubscript(_ obj: Test) -> [Any] {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $Test):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $Test, #Test.subscript!getter.1.foreign : (Test) -> (Int) -> [Any], $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[COCOA_VAL:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, [[ARG]]) : $@convention(objc_method) (Int, Test) -> @autoreleased Optional<NSArray>
  // CHECK: [[CONVERT:%[0-9]+]] = function_ref @$SSa10FoundationE36_unconditionallyBridgeFromObjectiveCySayxGSo7NSArrayCSgFZ
  // CHECK: [[ARRAY_META:%[0-9]+]] = metatype $@thin Array<Any>.Type,
  // CHECK: [[RESULT:%[0-9]+]] = apply [[CONVERT]]<Any>([[COCOA_VAL]], [[ARRAY_META]])
  // CHECK-NOT: destroy_value [[ARG]] : $Test
  // CHECK: return [[RESULT]] : $Array<Any>
  return obj[0]
} // CHECK: } // end sil function '$S20objc_bridged_results20testNonnullSubscriptySayypGSo4TestCF'


// CHECK-LABEL: sil hidden @$S20objc_bridged_results19testPerformSelectoryySo8NSObjectCF
func testPerformSelector(_ obj: NSObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $NSObject):
  // CHECK: [[METHOD:%[0-9]+]] = objc_method [[ARG]] : $NSObject, #NSObject.perform!1.foreign
  // CHECK: [[RESULT:%[0-9]+]] = apply [[METHOD]]({{%[0-9]+}}, {{%[0-9]+}}, [[ARG]])
  _ = obj.perform("foo", with: nil)
  // CHECK-NOT: {{(retain|release).+}}[[RESULT]]
  // CHECK-NOT: {{(retain|release).+}}[[RESULT]]
} // CHECK: } // end sil function '$S20objc_bridged_results19testPerformSelectoryySo8NSObjectCF'

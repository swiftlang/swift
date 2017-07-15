// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import OtherSubscripts
import objc_generics

func useNS(_ : NSString) {}
func useOptNS(_ : NSString?) {}
func makeNS() -> NSString { return "help" as NSString }
func makeOptNS() -> NSString? { return nil }

/*** Return values ***********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole16testMethodResultySo10DummyClassC5dummy_tF
func testMethodResult(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.fetchNullableString() as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.fetchNullproneString() as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.fetchNonnullString() as NSString?)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonNullMethodResultySo10DummyClassC5dummy_tF
func testNonNullMethodResult(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.fetchNonnullString() as NSString)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole22testForcedMethodResultySo10DummyClassC5dummy_tF
func testForcedMethodResult(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.fetchNullproneString() as NSString)
}

/*** Property loads **********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole17testPropertyValueySo10DummyClassC5dummy_tF
func testPropertyValue(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nullableStringProperty as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nullproneStringProperty as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nonnullStringProperty as NSString?)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNonNullPropertyValueySo10DummyClassC5dummy_tF
func testNonNullPropertyValue(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.nonnullStringProperty as NSString)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testForcedPropertyValueySo10DummyClassC5dummy_tF
func testForcedPropertyValue(dummy: DummyClass) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.nullproneStringProperty as NSString)
}

/*** Subscript loads *********************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonnullSubscriptGetySo0eF0C6object_yXl5indextF
func testNonnullSubscriptGet(object: NonnullSubscript, index: AnyObject) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useNS(object[index] as NSString)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNullableSubscriptGetySo0eF0C6object_yXl5indextF
func testNullableSubscriptGet(object: NullableSubscript, index: AnyObject) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole25testNullproneSubscriptGetySo0eF0C6object_yXl5indextF
func testNullproneSubscriptGet(object: NullproneSubscript, index: AnyObject) {
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)

  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : $NSString):
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useNS(object[index] as NSString)
}

/*** Call arguments **********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole18testMethodArgumentySo10DummyClassC5dummy_tF 
func testMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNonnullString(makeNS() as String)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeNS() as String)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeNS() as String)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole28testValueToOptMethodArgumentySo10DummyClassC5dummy_tF
func testValueToOptMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeNS() as String?)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeNS() as String?)
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole09testOptToE14MethodArgumentySo10DummyClassC5dummy_tF
func testOptToOptMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeOptNS() as String?)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeOptNS() as String?)
}

/*** Property assignments ****************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole18testPropertySetterySo10DummyClassC5dummy_tF 
func testPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nonnullStringProperty = makeNS() as String

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeNS() as String

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeNS() as String
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole28testValueToOptPropertySetterySo10DummyClassC5dummy_tF
func testValueToOptPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeNS() as String?

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeNS() as String?
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole09testOptToE14PropertySetterySo10DummyClassC5dummy_tF
func testOptToOptPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeOptNS() as String?

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = class_method
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeOptNS() as String?
}

/*** Subscript assignments ***************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonnullSubscriptSetySo0eF0C6object_yXl5indextF
func testNonnullSubscriptSet(object: NonnullSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNullableSubscriptSetySo0eF0C6object_yXl5indextF
func testNullableSubscriptSet(object: NullableSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String?

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeOptNS() as String?
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole25testNullproneSubscriptSetySo0eF0C6object_yXl5indextF 
func testNullproneSubscriptSet(object: NullproneSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String?

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[METHOD:%.*]] = class_method
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeOptNS() as String?
}

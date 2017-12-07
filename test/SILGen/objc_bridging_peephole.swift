// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import OtherSubscripts
import objc_generics

func useNS(_ : NSString) {}
func useOptNS(_ : NSString?) {}
func makeNS() -> NSString { return "help" as NSString }
func makeOptNS() -> NSString? { return nil }

func useAnyObject(_: AnyObject) {}
func useOptAnyObject(_: AnyObject?) {}

/*** Return values ***********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole16testMethodResultySo10DummyClassC5dummy_tF
func testMethodResult(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNullableString() as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNullproneString() as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNonnullString() as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNullableString() as AnyObject?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNullproneString() as AnyObject?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNonnullString() as AnyObject?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonNullMethodResultySo10DummyClassC5dummy_tF
func testNonNullMethodResult(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(dummy.fetchNonnullString() as NSString)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb3:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb4([[RESULT:%.*]] : @owned $NSString):
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = init_existential_ref [[RESULT]] : $NSString : $NSString, $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole12useAnyObjectyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useAnyObject(dummy.fetchNonnullString() as AnyObject)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole22testForcedMethodResultySo10DummyClassC5dummy_tF
func testForcedMethodResult(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(dummy.fetchNullproneString() as NSString)

  //   This is not a force.
  //   TODO: we could do it more efficiently than this, though
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb3([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:    bb4:
  // CHECK:      enum $Optional<String>, #Optional.none
  // CHECK:    bb5([[OPTSTRING:%.*]] : @owned $Optional<String>):
  // CHECK:      [[BRIDGE:%.*]] = function_ref @_T0Sq19_bridgeToObjectiveCyXlyF
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<String>
  // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[OPTSTRING]]
  // CHECK-NEXT: store_borrow [[BORROW]] to [[TEMP]] : $*Optional<String>
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE]]<String>([[TEMP]])
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole12useAnyObjectyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[BORROW]] from [[OPTSTRING]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  useAnyObject(dummy.fetchNullproneString() as AnyObject)

  // CHECK:      return
}

/*** Property loads **********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole17testPropertyValueySo10DummyClassC5dummy_tF
func testPropertyValue(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nullableStringProperty as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nullproneStringProperty as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptNS(dummy.nonnullStringProperty as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptAnyObject(dummy.nullableStringProperty as AnyObject?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptAnyObject(dummy.nullproneStringProperty as AnyObject?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole15useOptAnyObjectyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useOptAnyObject(dummy.nonnullStringProperty as AnyObject?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNonNullPropertyValueySo10DummyClassC5dummy_tF
func testNonNullPropertyValue(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.nonnullStringProperty as NSString)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb3:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb4([[RESULT:%.*]] : @owned $NSString):
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = init_existential_ref [[RESULT]] : $NSString : $NSString, $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole12useAnyObjectyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useAnyObject(dummy.nonnullStringProperty as AnyObject)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testForcedPropertyValueySo10DummyClassC5dummy_tF
func testForcedPropertyValue(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart_Bw01_E6LengthBi1_01_E7IsASCIIBw5_linetF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useNS(dummy.nullproneStringProperty as NSString)

  //   This is not a force.
  //   TODO: we could do it more efficiently than this, though
  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb3([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
  // CHECK:    bb4:
  // CHECK:      enum $Optional<String>, #Optional.none
  // CHECK:    bb5([[OPTSTRING:%.*]] : @owned $Optional<String>):
  // CHECK:      [[BRIDGE:%.*]] = function_ref @_T0Sq19_bridgeToObjectiveCyXlyF
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<String>
  // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[OPTSTRING]]
  // CHECK-NEXT: store_borrow [[BORROW]] to [[TEMP]] : $*Optional<String>
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE]]<String>([[TEMP]])
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole12useAnyObjectyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK-NEXT: end_borrow [[BORROW]] from [[OPTSTRING]]
  // CHECK-NEXT: dealloc_stack [[TEMP]]
  // CHECK-NEXT: destroy_value [[OPTSTRING]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  useAnyObject(dummy.nullproneStringProperty as AnyObject)

  // CHECK:      return
}

/*** Subscript loads *********************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonnullSubscriptGetySo0eF0C6object_yXl5indextF
func testNonnullSubscriptGet(object: NonnullSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useNS(object[index] as NSString)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNullableSubscriptGetySo0eF0C6object_yXl5indextF
func testNullableSubscriptGet(object: NullableSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole25testNullproneSubscriptGetySo0eF0C6object_yXl5indextF
func testNullproneSubscriptGet(object: NullproneSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole8useOptNSySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useOptNS(object[index] as NSString?)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @_T0s30_diagnoseUnexpectedNilOptionalyBp14_filenameStart
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @_T022objc_bridging_peephole5useNSySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  // CHECK:      end_borrow [[SELF]] from %0
  useNS(object[index] as NSString)

  // CHECK:      return
}

/*** Call arguments **********************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole18testMethodArgumentySo10DummyClassC5dummy_tF 
func testMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNonnullString(makeNS() as String)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeNS() as String)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeNS() as String)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole28testValueToOptMethodArgumentySo10DummyClassC5dummy_tF
func testValueToOptMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeNS() as String?)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeNS() as String?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole09testOptToE14MethodArgumentySo10DummyClassC5dummy_tF
func testOptToOptMethodArgument(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullableString(makeOptNS() as String?)

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.takeNullproneString(makeOptNS() as String?)

  // CHECK:      return
}

/*** Property assignments ****************************************************/

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole18testPropertySetterySo10DummyClassC5dummy_tF 
func testPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nonnullStringProperty = makeNS() as String

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeNS() as String

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeNS() as String

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole28testValueToOptPropertySetterySo10DummyClassC5dummy_tF
func testValueToOptPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeNS() as String?

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeNS() as String?

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole09testOptToE14PropertySetterySo10DummyClassC5dummy_tF
func testOptToOptPropertySetter(dummy: DummyClass) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullableStringProperty = makeOptNS() as String?

  // CHECK-NEXT: [[SELF:%.*]] = begin_borrow %0
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  // CHECK-NEXT: end_borrow [[SELF]] from %0
  dummy.nullproneStringProperty = makeOptNS() as String?

  // CHECK:      return
}

/*** Subscript assignments ***************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole23testNonnullSubscriptSetySo0eF0C6object_yXl5indextF
func testNonnullSubscriptSet(object: NonnullSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole24testNullableSubscriptSetySo0eF0C6object_yXl5indextF
func testNullableSubscriptSet(object: NullableSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String?

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeOptNS() as String?

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole25testNullproneSubscriptSetySo0eF0C6object_yXl5indextF 
func testNullproneSubscriptSet(object: NullproneSubscript, index: AnyObject) {
  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeNS() as String?

  // CHECK:      [[SELF:%.*]] = begin_borrow %0
  // CHECK:      [[MAKE:%.*]] = function_ref @_T022objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @_T0s27_bridgeAnythingToObjectiveCyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  // CHECK:      end_borrow [[SELF]] from %0
  object[index] = makeOptNS() as String?

  // CHECK:      return
}

/*** Bugfixes ***************************************************************/

protocol P {
  var title : String { get }
}

func foo(p: P) {
  DummyClass().takeNullableString(p.title)
}

// rdar://35402853
//   Make sure that we don't peephole AnyObject? -> Any? -> AnyObject naively.
// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole017testOptionalToNonE6BridgeyyF
func testOptionalToNonOptionalBridge() {
  // CHECK: apply {{.*}}() : $@convention(c) () -> @autoreleased Optional<AnyObject>
  // CHECK: function_ref @_T0s018_bridgeAnyObjectToB0ypyXlSgF :
  // CHECK: [[T0:%.*]] = function_ref @_T0Sq19_bridgeToObjectiveCyXlyF
  // CHECK: apply [[T0]]<Any>
  useAnyObject(returnNullableId() as AnyObject)
} // CHECK: end sil function '_T022objc_bridging_peephole017testOptionalToNonE6BridgeyyF'

// CHECK-LABEL: sil hidden @_T022objc_bridging_peephole34testBlockToOptionalAnyObjectBridgeyyyXB5block_tF
func testBlockToOptionalAnyObjectBridge(block: @escaping @convention(block) () -> ()) {
  // CHECK:      [[T0:%.*]] = begin_borrow {{%.*}} : $@convention(block) () -> ()
  // CHECK-NEXT: [[T1:%.*]] = copy_value [[T0]]
  // CHECK-NEXT: [[REF:%.*]] = unchecked_ref_cast [[T1]] : $@convention(block) () -> () to $AnyObject
  // CHECK-NEXT: [[OPTREF:%.*]] = enum $Optional<AnyObject>, #Optional.some!enumelt.1, [[REF]] : $AnyObject
  // CHECK-NEXT: end_borrow [[T0]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[FN:%.*]] = function_ref @takeNullableId : $@convention(c) (Optional<AnyObject>) -> ()
  // CHECK-NEXT: apply [[FN]]([[OPTREF]])
  // CHECK-NEXT: destroy_value [[OPTREF]]
  takeNullableId(block as Any)
}

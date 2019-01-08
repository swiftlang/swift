
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -module-name objc_bridging_peephole -enable-sil-ownership %s | %FileCheck %s
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

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole16testMethodResult5dummyySo10DummyClassC_tF
func testMethodResult(dummy: DummyClass) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $DummyClass, #DummyClass.fetchNullableString!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNullableString() as NSString?)

  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $DummyClass, #DummyClass.fetchNullproneString!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNullproneString() as NSString?)

  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $DummyClass, #DummyClass.fetchNonnullString!1.foreign
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(dummy.fetchNonnullString() as NSString?)

  // CHECK: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNullableString() as AnyObject?)

  // CHECK: [[METHOD:%.*]] = objc_method 
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNullproneString() as AnyObject?)

  // CHECK: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.fetchNonnullString() as AnyObject?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole23testNonNullMethodResult5dummyySo10DummyClassC_tF
func testNonNullMethodResult(dummy: DummyClass) {
  // CHECK: bb0([[ARG:%.*]] @guaranteed $DummyClass):
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  //
  // CHECK:    bb1:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(dummy.fetchNonnullString() as NSString)

  // CHECK: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb3:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb4([[RESULT:%.*]] : @owned $NSString):
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = init_existential_ref [[RESULT]] : $NSString : $NSString, $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole12useAnyObjectyyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  useAnyObject(dummy.fetchNonnullString() as AnyObject)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole22testForcedMethodResult5dummyySo10DummyClassC_tF
// CHECK: bb0([[SELF:%.*]] : @guaranteed $DummyClass):
func testForcedMethodResult(dummy: DummyClass) {
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(dummy.fetchNullproneString() as NSString)

  //   This is not a force.
  //   TODO: we could do it more efficiently than this, though
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK-NEXT: switch_enum [[RESULT]]
  //
  // CHECK:    bb3([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  //
  // CHECK:    bb4:
  // CHECK:      enum $Optional<String>, #Optional.none
  //
  // CHECK:    bb5([[OPTSTRING:%.*]] : @owned $Optional<String>):
  // CHECK:      [[BRIDGE:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<String>
  // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[OPTSTRING]]
  // CHECK-NEXT: store_borrow [[BORROW]] to [[TEMP]] : $*Optional<String>
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE]]<String>([[TEMP]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole12useAnyObjectyyyXlF
  // CHECK:      apply [[USE]]([[ANYOBJECT]])
  useAnyObject(dummy.fetchNullproneString() as AnyObject)

  // CHECK:      return
}

/*** Property loads **********************************************************/

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole17testPropertyValue5dummyySo10DummyClassC_tF
// CHECK: bb0([[SELF:%.*]] : @guaranteed $DummyClass):
func testPropertyValue(dummy: DummyClass) {
  // CHECK: [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK:      apply [[USE]]([[RESULT]])
  useOptNS(dummy.nullableStringProperty as NSString?)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK:      apply [[USE]]([[RESULT]])
  useOptNS(dummy.nullproneStringProperty as NSString?)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK:      apply [[USE]]([[RESULT]])
  useOptNS(dummy.nonnullStringProperty as NSString?)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK:      apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.nullableStringProperty as AnyObject?)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK:      apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.nullproneStringProperty as AnyObject?)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      [[ANYOBJECT:%.*]] = unchecked_ref_cast [[RESULT]] : $Optional<NSString> to $Optional<AnyObject>
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole15useOptAnyObjectyyyXlSgF
  // CHECK:      apply [[USE]]([[ANYOBJECT]])
  useOptAnyObject(dummy.nonnullStringProperty as AnyObject?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole24testNonNullPropertyValue5dummyySo10DummyClassC_tF
func testNonNullPropertyValue(dummy: DummyClass) {
  // CHECK:    bb0([[SELF:%.*]] : @guaranteed $DummyClass):
  // CHECK:       [[METHOD:%.*]] = objc_method
  // CHECK:       [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:       switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK:      apply [[USE]]([[RESULT]])
  useNS(dummy.nonnullStringProperty as NSString)

  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      switch_enum [[RESULT]]
  // CHECK:    bb3:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb4([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[ANYOBJECT:%.*]] = init_existential_ref [[RESULT]] : $NSString : $NSString, $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole12useAnyObjectyyyXlF
  // CHECK:      apply [[USE]]([[ANYOBJECT]])
  useAnyObject(dummy.nonnullStringProperty as AnyObject)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole23testForcedPropertyValue5dummyySo10DummyClassC_tF
func testForcedPropertyValue(dummy: DummyClass) {
  // CHECK:    bb0([[ARG:%.*]] : @guaranteed $DummyClass):
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      switch_enum [[RESULT]]
  // CHECK:    bb1:
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb2([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK:      apply [[USE]]([[RESULT]])
  useNS(dummy.nullproneStringProperty as NSString)

  //   This is not a force.
  //   TODO: we could do it more efficiently than this, though
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:      switch_enum [[RESULT]]
  // CHECK:    bb3([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
  // CHECK:    bb4:
  // CHECK:      enum $Optional<String>, #Optional.none
  // CHECK:    bb5([[OPTSTRING:%.*]] : @owned $Optional<String>):
  // CHECK:      [[BRIDGE:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<String>
  // CHECK-NEXT: [[BORROW:%.*]] = begin_borrow [[OPTSTRING]]
  // CHECK-NEXT: store_borrow [[BORROW]] to [[TEMP]] : $*Optional<String>
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE]]<String>([[TEMP]])
  // CHECK:      dealloc_stack [[TEMP]]
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole12useAnyObjectyyyXlF
  // CHECK-NEXT: apply [[USE]]([[ANYOBJECT]])
  // CHECK:      destroy_value [[OPTSTRING]]
  useAnyObject(dummy.nullproneStringProperty as AnyObject)

  // CHECK:      return
}

/*** Subscript loads *********************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole23testNonnullSubscriptGet6object5indexySo0eF0C_yXltF
func testNonnullSubscriptGet(object: NonnullSubscript, index: AnyObject) {
  // CHECK:   bb0([[SELF:%.*]] : @guaranteed $NonnullSubscript,
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(object[index] as NSString?)

  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(object[index] as NSString)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole24testNullableSubscriptGet6object5indexySo0eF0C_yXltF
func testNullableSubscriptGet(object: NullableSubscript, index: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $NullableSubscript,
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(object[index] as NSString?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole25testNullproneSubscriptGet6object5indexySo0eF0C_yXltF
func testNullproneSubscriptGet(object: NullproneSubscript, index: AnyObject) {
  // CHECK:   bb0([[ARG:%.*]] : @guaranteed $NullproneSubscript,
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole8useOptNSyySo8NSStringCSgF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useOptNS(object[index] as NSString?)

  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      [[RESULT:%.*]] = apply [[METHOD]]([[INDEX]], [[SELF]])
  // CHECK-NEXT: destroy_value [[INDEX]] : $AnyObject
  // CHECK-NEXT: switch_enum [[RESULT]]
  // CHECK:      function_ref @$ss30_diagnoseUnexpectedNilOptional14_filenameStart01_E6Length01_E7IsASCII5_line17_isImplicitUnwrapyBp_BwBi1_BwBi1_tF
  // CHECK:    bb{{[0-9]+}}([[RESULT:%.*]] : @owned $NSString):
  // CHECK:      [[USE:%.*]] = function_ref @$s22objc_bridging_peephole5useNSyySo8NSStringCF
  // CHECK-NEXT: apply [[USE]]([[RESULT]])
  useNS(object[index] as NSString)

  // CHECK:      return
}

/*** Call arguments **********************************************************/

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole18testMethodArgument5dummyySo10DummyClassC_tF
func testMethodArgument(dummy: DummyClass) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $DummyClass):
  // CHECK:      // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.takeNonnullString(makeNS() as String)

  // CHECK:      // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.takeNullableString(makeNS() as String)

  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.takeNullproneString(makeNS() as String)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole28testValueToOptMethodArgument5dummyySo10DummyClassC_tF
func testValueToOptMethodArgument(dummy: DummyClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.takeNullableString(makeNS() as String?)

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.takeNullproneString(makeNS() as String?)

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole09testOptToE14MethodArgument5dummyySo10DummyClassC_tF
func testOptToOptMethodArgument(dummy: DummyClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.takeNullableString(makeOptNS() as String?)

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.takeNullproneString(makeOptNS() as String?)

  // CHECK:      return
}

/*** Property assignments ****************************************************/

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole18testPropertySetter5dummyySo10DummyClassC_tF
func testPropertySetter(dummy: DummyClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.nonnullStringProperty = makeNS() as String

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.nullableStringProperty = makeNS() as String

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.nullproneStringProperty = makeNS() as String

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole28testValueToOptPropertySetter5dummyySo10DummyClassC_tF
func testValueToOptPropertySetter(dummy: DummyClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.nullableStringProperty = makeNS() as String?

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]] : $NSString
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[OPTARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPTARG]]
  dummy.nullproneStringProperty = makeNS() as String?

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole09testOptToE14PropertySetter5dummyySo10DummyClassC_tF
func testOptToOptPropertySetter(dummy: DummyClass) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $DummyClass):
  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.nullableStringProperty = makeOptNS() as String?

  // CHECK: [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[METHOD:%.*]] = objc_method
  // CHECK-NEXT: apply [[METHOD]]([[ARG]], [[SELF]])
  // CHECK-NEXT: destroy_value [[ARG]]
  dummy.nullproneStringProperty = makeOptNS() as String?

  // CHECK:      return
}

/*** Subscript assignments ***************************************************/

// FIXME: apply peepholes to indices, too!

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole23testNonnullSubscriptSet6object5indexySo0eF0C_yXltF
func testNonnullSubscriptSet(object: NonnullSubscript, index: AnyObject) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $NonnullSubscript,
  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  object[index] = makeNS() as String

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole24testNullableSubscriptSet6object5indexySo0eF0C_yXltF
func testNullableSubscriptSet(object: NullableSubscript, index: AnyObject) {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $NullableSubscript,
  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  object[index] = makeNS() as String

  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  object[index] = makeNS() as String?

  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
  object[index] = makeOptNS() as String?

  // CHECK:      return
}

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole25testNullproneSubscriptSet6object5indexySo0eF0C_yXltF
func testNullproneSubscriptSet(object: NullproneSubscript, index: AnyObject) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $NullproneSubscript,
  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  object[index] = makeNS() as String

  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole6makeNSSo8NSStringCyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK-NEXT: [[OPTARG:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[ARG]]
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[OPTARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[OPTARG]]
  object[index] = makeNS() as String?

  // CHECK:      [[MAKE:%.*]] = function_ref @$s22objc_bridging_peephole9makeOptNSSo8NSStringCSgyF
  // CHECK-NEXT: [[ARG:%.*]] = apply [[MAKE]]()
  // CHECK:      [[BRIDGE_TO_ID:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[INDEX:%.*]] = apply [[BRIDGE_TO_ID]]
  // CHECK:      [[METHOD:%.*]] = objc_method
  // CHECK:      apply [[METHOD]]([[ARG]], [[INDEX]], [[SELF]])
  // CHECK:      destroy_value [[INDEX]]
  // CHECK:      destroy_value [[ARG]]
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
// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole017testOptionalToNonE6BridgeyyF
func testOptionalToNonOptionalBridge() {
  // CHECK: apply {{.*}}() : $@convention(c) () -> @autoreleased Optional<AnyObject>
  // CHECK: function_ref @$ss018_bridgeAnyObjectToB0yypyXlSgF :
  // CHECK: [[T0:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK: apply [[T0]]<Any>
  useAnyObject(returnNullableId() as AnyObject)
} // CHECK: end sil function '$s22objc_bridging_peephole017testOptionalToNonE6BridgeyyF'

// CHECK-LABEL: sil hidden @$s22objc_bridging_peephole34testBlockToOptionalAnyObjectBridge5blockyyyXB_tF
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

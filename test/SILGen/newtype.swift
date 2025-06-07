// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk -I %t) -Xllvm -sil-print-types -module-name newtype -I %S/Inputs -I %S/Inputs -I %S/../IDE/Inputs/custom-modules -enable-objc-interop -enable-source-import %s | %FileCheck %s -check-prefix=CHECK-RAW
// RUN: %target-swift-emit-sil(mock-sdk: %clang-importer-sdk -I %t) -Xllvm -sil-print-types -module-name newtype -I %S/Inputs -I %S/Inputs -I %S/../IDE/Inputs/custom-modules -enable-objc-interop -enable-source-import %s | %FileCheck %s -check-prefix=CHECK-CANONICAL

// REQUIRES: objc_interop

import Newtype

// CHECK-CANONICAL-LABEL: sil hidden @$s7newtype17createErrorDomain{{[_0-9a-zA-Z]*}}F
// CHECK-CANONICAL: bb0([[STR:%[0-9]+]] : $String)
func createErrorDomain(str: String) -> ErrorDomain {
  // CHECK-CANONICAL: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
  // CHECK-CANONICAL-NEXT: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[STR]])
  // CHECK-CANONICAL: struct $ErrorDomain ([[BRIDGED]] : $NSString)
  return ErrorDomain(rawValue: str)
}

// CHECK-RAW-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo14SNTErrorDomaina8rawValueABSS_tcfC
// CHECK-RAW: bb0([[STR:%[0-9]+]] : @owned $String,
// CHECK-RAW: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var ErrorDomain }
// CHECK-RAW: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [rootself] [[SELF_BOX]]
// CHECK-RAW: [[SELF_BOX_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
// CHECK-RAW: [[PB_BOX:%[0-9]+]] = project_box [[SELF_BOX_LIFETIME]]
// CHECK-RAW: [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
// CHECK-RAW: [[COPIED_STR:%.*]] = copy_value [[BORROWED_STR]]
// CHECK-RAW: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
// CHECK-RAW: [[BORROWED_COPIED_STR:%.*]] = begin_borrow [[COPIED_STR]]
// CHECK-RAW: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[BORROWED_COPIED_STR]])
// CHECK-RAW: end_borrow [[BORROWED_COPIED_STR]]
// CHECK-RAW: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB_BOX]]
// CHECK-RAW: [[RAWVALUE_ADDR:%[0-9]+]] = struct_element_addr [[WRITE]]
// CHECK-RAW: assign [[BRIDGED]] to [[RAWVALUE_ADDR]]
// CHECK-RAW: end_borrow [[BORROWED_STR]]

func getRawValue(ed: ErrorDomain) -> String {
  return ed.rawValue
}

// CHECK-RAW-LABEL: sil shared [serialized] [ossa] @$sSo14SNTErrorDomaina8rawValueSSvg
// CHECK-RAW: bb0([[SELF:%[0-9]+]] : @guaranteed $ErrorDomain):
// CHECK-RAW: [[STORED_VALUE:%[0-9]+]] = struct_extract [[SELF]] : $ErrorDomain, #ErrorDomain._rawValue
// CHECK-RAW: [[STORED_VALUE_COPY:%.*]] = copy_value [[STORED_VALUE]]
// CHECK-RAW: [[BRIDGE_FN:%[0-9]+]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK-RAW: [[OPT_STORED_VALUE_COPY:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt, [[STORED_VALUE_COPY]]
// CHECK-RAW: [[STRING_META:%[0-9]+]] = metatype $@thin String.Type
// CHECK-RAW: [[STRING_RESULT:%[0-9]+]] = apply [[BRIDGE_FN]]([[OPT_STORED_VALUE_COPY]], [[STRING_META]])
// CHECK-RAW: return [[STRING_RESULT]]

class ObjCTest {
  // CHECK-RAW-LABEL: sil hidden [ossa] @$s7newtype8ObjCTestC19optionalPassThroughySo14SNTErrorDomainaSgAGF : $@convention(method) (@guaranteed Optional<ErrorDomain>, @guaranteed ObjCTest) -> @owned Optional<ErrorDomain> {
  // CHECK-RAW: sil private [thunk] [ossa] @$s7newtype8ObjCTestC19optionalPassThroughySo14SNTErrorDomainaSgAGFTo : $@convention(objc_method) (Optional<ErrorDomain>, ObjCTest) -> @autoreleased Optional<ErrorDomain> {
  @objc func optionalPassThrough(_ ed: ErrorDomain?) -> ErrorDomain? {
    return ed
  }  

  // CHECK-RAW-LABEL: sil hidden [ossa] @$s7newtype8ObjCTestC18integerPassThroughySo5MyIntaAFF : $@convention(method) (MyInt, @guaranteed ObjCTest) -> MyInt {
  // CHECK-RAW: sil private [thunk] [ossa] @$s7newtype8ObjCTestC18integerPassThroughySo5MyIntaAFFTo : $@convention(objc_method) (MyInt, ObjCTest) -> MyInt {
  @objc func integerPassThrough(_ ed: MyInt) -> MyInt {
    return ed
  }  
}

// These use a bridging conversion with a specialization of a generic witness.
// CHECK-RAW-LABEL: sil hidden [ossa] @$s7newtype15bridgeToNewtypeSo8MyStringayF
func bridgeToNewtype() -> MyString {
// CHECK-RAW: [[STRING:%.*]] = apply
// CHECK-RAW: [[TO_NS:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-RAW: [[BORROW:%.*]] = begin_borrow [[STRING]]
// CHECK-RAW: [[NS:%.*]] = apply [[TO_NS]]([[BORROW]])
// CHECK-RAW: [[TO_MY:%.*]] = function_ref @$ss20_SwiftNewtypeWrapperPss21_ObjectiveCBridgeable8RawValueRpzrlE026_unconditionallyBridgeFromD1CyxAD_01_D5CTypeQZSgFZ : $@convention(method) <τ_0_0 where τ_0_0 : _SwiftNewtypeWrapper, τ_0_0.RawValue : _ObjectiveCBridgeable> (@guaranteed Optional<τ_0_0.RawValue._ObjectiveCType>, @thick τ_0_0.Type)
// CHECK-RAW: [[OPTNS:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt, [[NS]]
// CHECK-RAW: [[META:%.*]] = metatype $@thick MyString.Type
// CHECK-RAW: apply [[TO_MY]]<MyString>({{.*}}, [[OPTNS]], [[META]])
  return "foo" as NSString as MyString
}

// CHECK-RAW-LABEL: sil hidden [ossa] @$s7newtype17bridgeFromNewtype6stringSSSo8MyStringa_tF
func bridgeFromNewtype(string: MyString) -> String {
// CHECK-RAW: [[FROM_MY:%.*]] = function_ref @$ss20_SwiftNewtypeWrapperPss21_ObjectiveCBridgeable8RawValueRpzrlE09_bridgeToD1CAD_01_D5CTypeQZyF : $@convention(method) <τ_0_0 where τ_0_0 : _SwiftNewtypeWrapper, τ_0_0.RawValue : _ObjectiveCBridgeable> (@in_guaranteed τ_0_0) -> @owned τ_0_0.RawValue._ObjectiveCType
// CHECK-RAW: [[NS:%.*]] = apply [[FROM_MY]]<MyString>(
// CHECK-RAW: [[FROM_NS:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK-RAW: [[OPTNS:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt, [[NS]]
// CHECK-RAW: [[META:%.*]] = metatype $@thin String.Type
// CHECK-RAW: apply [[FROM_NS]]([[OPTNS]], [[META]])
  return string as NSString as String
}

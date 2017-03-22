// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import  %s | %FileCheck %s -check-prefix=CHECK-RAW

// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import  %s | %FileCheck %s -check-prefix=CHECK-CANONICAL

// REQUIRES: objc_interop

import Newtype

// CHECK-CANONICAL-LABEL: sil hidden @_T07newtype17createErrorDomain{{[_0-9a-zA-Z]*}}F
// CHECK-CANONICAL: bb0([[STR:%[0-9]+]] : $String)
func createErrorDomain(str: String) -> ErrorDomain {
  // CHECK-CANONICAL: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
  // CHECK-CANONICAL-NEXT: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[STR]])
  // CHECK-CANONICAL: struct $ErrorDomain ([[BRIDGED]] : $NSString)
  return ErrorDomain(rawValue: str)
}

// CHECK-RAW-LABEL: sil shared [transparent] [fragile] @_T0SC11ErrorDomainVABSS8rawValue_tcfC
// CHECK-RAW: bb0([[STR:%[0-9]+]] : $String,
// CHECK-RAW: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var ErrorDomain }, var, name "self"
// CHECK-RAW: [[SELF:%[0-9]+]] = project_box [[SELF_BOX]]
// CHECK-RAW: [[UNINIT_SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF]]
// CHECK-RAW: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
// CHECK-RAW: [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
// CHECK-RAW: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[BORROWED_STR]])
// CHECK-RAW: end_borrow [[BORROWED_STR]] from [[STR]]
// CHECK-RAW: [[RAWVALUE_ADDR:%[0-9]+]] = struct_element_addr [[UNINIT_SELF]]
// CHECK-RAW: assign [[BRIDGED]] to [[RAWVALUE_ADDR]]

func getRawValue(ed: ErrorDomain) -> String {
  return ed.rawValue
}

// CHECK-RAW-LABEL: sil shared @_T0SC11ErrorDomainV8rawValueSSfg
// CHECK-RAW: bb0([[SELF:%[0-9]+]] : $ErrorDomain):
// CHECK-RAW: [[FORCE_BRIDGE:%[0-9]+]] = function_ref @_forceBridgeFromObjectiveC_bridgeable
// CHECK-RAW: [[STRING_RESULT_ADDR:%[0-9]+]] = alloc_stack $String
// CHECK-RAW: [[STORED_VALUE:%[0-9]+]] = struct_extract [[SELF]] : $ErrorDomain, #ErrorDomain._rawValue
// CHECK-RAW: [[STORED_VALUE_COPY:%.*]] = copy_value [[STORED_VALUE]]
// CHECK-RAW: [[STRING_META:%[0-9]+]] = metatype $@thick String.Type
// CHECK-RAW: apply [[FORCE_BRIDGE]]<String>([[STRING_RESULT_ADDR]], [[STORED_VALUE_COPY]], [[STRING_META]])
// CHECK-RAW: [[STRING_RESULT:%[0-9]+]] = load [take] [[STRING_RESULT_ADDR]]
// CHECK-RAW: return [[STRING_RESULT]]

class ObjCTest {
  // CHECK-RAW-LABEL: sil hidden @_T07newtype8ObjCTestC19optionalPassThroughSC11ErrorDomainVSgAGF : $@convention(method) (@owned Optional<ErrorDomain>, @guaranteed ObjCTest) -> @owned Optional<ErrorDomain> {
  // CHECK-RAW: sil hidden [thunk] @_T07newtype8ObjCTestC19optionalPassThroughSC11ErrorDomainVSgAGFTo : $@convention(objc_method) (Optional<ErrorDomain>, ObjCTest) -> Optional<ErrorDomain> {
  @objc func optionalPassThrough(_ ed: ErrorDomain?) -> ErrorDomain? {
    return ed
  }  

  // CHECK-RAW-LABEL: sil hidden @_T07newtype8ObjCTestC18integerPassThroughSC5MyIntVAFF : $@convention(method) (MyInt, @guaranteed ObjCTest) -> MyInt {
  // CHECK-RAW: sil hidden [thunk] @_T07newtype8ObjCTestC18integerPassThroughSC5MyIntVAFFTo : $@convention(objc_method) (MyInt, ObjCTest) -> MyInt {
  @objc func integerPassThrough(_ ed: MyInt) -> MyInt {
    return ed
  }  
}


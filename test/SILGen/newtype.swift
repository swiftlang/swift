// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-swift-newtype  %s | %FileCheck %s -check-prefix=CHECK-RAW

// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-swift-newtype  %s | %FileCheck %s -check-prefix=CHECK-CANONICAL

// REQUIRES: objc_interop

import Newtype

// CHECK-CANONICAL-LABEL: sil hidden @_TF7newtype17createErrorDomain
// CHECK-CANONICAL: bb0([[STR:%[0-9]+]] : $String)
func createErrorDomain(str: String) -> ErrorDomain {
  // CHECK-CANONICAL: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
  // CHECK-CANONICAL-NEXT: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[STR]])
  // CHECK-CANONICAL: struct $ErrorDomain ([[BRIDGED]] : $NSString)
  return ErrorDomain(rawValue: str)
}

// CHECK-RAW-LABEL: sil shared [transparent] [fragile] @_TFVSC11ErrorDomainCfT8rawValueSS_S_
// CHECK-RAW: bb0([[STR:%[0-9]+]] : $String,
// CHECK-RAW: [[SELF_BOX:%[0-9]+]] = alloc_box $ErrorDomain, var, name "self"
// CHECK-RAW: [[SELF:%[0-9]+]] = project_box [[SELF_BOX]]
// CHECK-RAW: [[UNINIT_SELF:%[0-9]+]] = mark_uninitialized [rootself] [[SELF]]
// CHECK-RAW: [[BRIDGE_FN:%[0-9]+]] = function_ref @{{.*}}_bridgeToObjectiveC
// CHECK-RAW: [[BRIDGED:%[0-9]+]] = apply [[BRIDGE_FN]]([[STR]])
// CHECK-RAW: [[RAWVALUE_ADDR:%[0-9]+]] = struct_element_addr [[UNINIT_SELF]]
// CHECK-RAW: assign [[BRIDGED]] to [[RAWVALUE_ADDR]]

func getRawValue(ed: ErrorDomain) -> String {
  return ed.rawValue
}

// CHECK-RAW-LABEL: sil shared @_TFVSC11ErrorDomaing8rawValueSS
// CHECK-RAW: bb0([[SELF:%[0-9]+]] : $ErrorDomain):
// CHECK-RAW: [[FORCE_BRIDGE:%[0-9]+]] = function_ref @_forceBridgeFromObjectiveC_bridgeable 
// CHECK-RAW: [[STORED_VALUE:%[0-9]+]] = struct_extract [[SELF]] : $ErrorDomain, #ErrorDomain._rawValue
// CHECK-RAW: [[STRING_META:%[0-9]+]] = metatype $@thick String.Type
// CHECK-RAW: [[STRING_RESULT_ADDR:%[0-9]+]] = alloc_stack $String
// CHECK-RAW: apply [[FORCE_BRIDGE]]<String, NSString>([[STRING_RESULT_ADDR]], [[STORED_VALUE]], [[STRING_META]])
// CHECK-RAW: [[STRING_RESULT:%[0-9]+]] = load [[STRING_RESULT_ADDR]]
// CHECK-RAW: return [[STRING_RESULT]]

class ObjCTest {
  // CHECK-RAW-LABEL: sil hidden @_TFC7newtype8ObjCTest19optionalPassThroughfGSqVSC11ErrorDomain_GSqS1__ : $@convention(method) (@owned Optional<ErrorDomain>, @guaranteed ObjCTest) -> @owned Optional<ErrorDomain> {
  // CHECK-RAW: sil hidden [thunk] @_TToFC7newtype8ObjCTest19optionalPassThroughfGSqVSC11ErrorDomain_GSqS1__ : $@convention(objc_method) (Optional<ErrorDomain>, ObjCTest) -> Optional<ErrorDomain> {
  @objc func optionalPassThrough(_ ed: ErrorDomain?) -> ErrorDomain? {
    return ed
  }  

  // CHECK-RAW-LABEL: sil hidden @_TFC7newtype8ObjCTest18integerPassThroughfVSC5MyIntS1_ : $@convention(method) (MyInt, @guaranteed ObjCTest) -> MyInt {
  // CHECK-RAW: sil hidden [thunk] @_TToFC7newtype8ObjCTest18integerPassThroughfVSC5MyIntS1_ : $@convention(objc_method) (MyInt, ObjCTest) -> MyInt {
  @objc func integerPassThrough(_ ed: MyInt) -> MyInt {
    return ed
  }  
}


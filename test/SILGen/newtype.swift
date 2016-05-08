// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-swift-newtype  %s | FileCheck %s -check-prefix=CHECK-RAW

// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import -enable-swift-newtype  %s | FileCheck %s -check-prefix=CHECK-CANONICAL

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

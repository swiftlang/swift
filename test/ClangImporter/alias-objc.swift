// RUN: %target-typecheck-verify-swift %clang-importer-sdk -I %S/Inputs/custom-modules %s -enable-experimental-feature ImportMacroAliases
// RUN: %target-swift-frontend %clang-importer-sdk -I %S/Inputs/custom-modules -parse-as-library -module-name AliasObjc -Osize -emit-ir -o - %s -enable-experimental-feature ImportMacroAliases | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportMacroAliases

// expected-no-diagnostics

import AliasesObjc

// CHECK-DAG: @"OBJC_CLASS_REF_$_MyObjCClass" =
// CHECK-DAG: @"OBJC_CLASS_$_MyObjCClass" =
// CHECK-DAG: @{{.*_PROTOCOL_.*MyObjCProtocol"?}} = weak hidden
// CHECK-DAG: @"{{.*l_OBJC_LABEL_PROTOCOL_.*MyObjCProtocol}}" = weak hidden global ptr @{{.*_PROTOCOL_.*MyObjCProtocol"?}}
// CHECK-DAG: @"{{.*l_OBJC_PROTOCOL_REFERENCE_.*MyObjCProtocol}}" = weak hidden global ptr @{{.*_PROTOCOL_.*MyObjCProtocol"?}}

public func testObjCClass(y: ALIASED_OBJC_CLASS) -> AnyObject {
  return ALIASED_OBJC_CLASS()
}
// CHECK-DAG: define {{.*}}swiftcc {{.*}} @"$s9AliasObjc13testObjCClass1yyXlSo02MydE0C_tF"

public func testObjCProtocol(x: ALIASED_OBJC_PROTOCOL, y: AnyObject) -> Bool {
  return y is ALIASED_OBJC_PROTOCOL
}
// CHECK-LABEL: define {{.*}}swiftcc {{.*}} @"$s9AliasObjc16testObjCProtocol1x1ySbSo02MydE0_p_yXltF"
// CHECK:         %[[PROTO_REF:[0-9]+]] = load ptr, ptr @"{{.*l_OBJC_PROTOCOL_REFERENCE_.*MyObjCProtocol}}"
// CHECK:         store ptr %[[PROTO_REF]], ptr %[[PROTO_LIST:[a-zA-Z0-9_]+]]
// CHECK:         call ptr @swift_dynamicCastObjCProtocolConditional(ptr {{%[0-9]+}}, i64 1, ptr {{.*}}%[[PROTO_LIST]])

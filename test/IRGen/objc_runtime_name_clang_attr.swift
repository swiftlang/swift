// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objective-c-protocol-symbolic-references -emit-ir -import-objc-header %S/Inputs/objc_runtime_name_clang_attr.h %s | %FileCheck %s
// REQUIRES: objc_interop

// Use the runtime name for runtime instantiation
// CHECK-LABEL: @"$sSo16ObjCRuntimeNamedCSgMD" = {{.*}}@"symbolic So26ObjCRuntimeNameIsDifferentCSg"
public func getMetadata() -> Any.Type {
  return ObjCRuntimeNamed?.self
}
// CHECK-LABEL: @"$sSo21ObjCProtoRuntimeNamed_pSgMD" = {{.*}}@"symbolic So31ObjCProtoRuntimeNameIsDifferent_pSg"
public func getMetadata2() -> Any.Type {
  return ObjCProtoRuntimeNamed?.self
}

// Use the source name for symbols to avoid breaking ABI.
// CHECK-LABEL: define{{.*}}3fooyySo16ObjCRuntimeNamedCF
public func foo(_: ObjCRuntimeNamed) {}

// CHECK-LABEL: define{{.*}}3fooyySo21ObjCProtoRuntimeNamed_pF
public func foo(_: ObjCProtoRuntimeNamed) {}

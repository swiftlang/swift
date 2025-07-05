// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -enable-experimental-feature CImplementation \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -disable-objc-interop \
// RUN:   -F %clang-importer-sdk-path/frameworks %s \
// RUN:   -import-objc-header %S/Inputs/objc_implementation.h -emit-ir \
// RUN:   -target %target-future-triple > %t.ir
// RUN: %FileCheck --input-file %t.ir %s

// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_CDecl

@implementation @cdecl
public func implFunc(_ param: Int32) {}

@implementation @cdecl
public func implFuncCName(_ param: Int32) {}

@implementation @cdecl(implFuncRenamed_C)
public func implFuncRenamed_Swift(param: Int32) {}

public func fn() {
  implFunc(2)
  implFuncCName(3)
  implFuncRenamed_Swift(param: 4)
}

/// implFunc(_:)
// CHECK-LABEL: define{{.*}} void @implFunc

// FIXME: We'd like this to be internal or hidden, not public.
// CHECK: define{{.*}} swiftcc void @"$s20cdecl_implementation8implFuncyys5Int32VF"

/// inplFuncCName(_:)
// CHECK-LABEL: define{{.*}} void @"\01_implFuncAsmName"

// FIXME: We'd like this to be internal or hidden, not public.
// CHECK: define{{.*}} swiftcc void @"$s20cdecl_implementation13implFuncCNameyys5Int32VF"

/// fn()
// CHECK-LABEL: define{{.*}} swiftcc void @"$s20cdecl_implementation2fnyyF"
// CHECK:   call void @implFunc
// CHECK:   call void @"\01_implFuncAsmName"
// CHECK:   call void @implFuncRenamed_C
// CHECK:   ret void
// CHECK: }


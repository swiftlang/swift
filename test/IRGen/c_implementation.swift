// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -disable-objc-interop \
// RUN:   -F %clang-importer-sdk-path/frameworks %s \
// RUN:   -import-objc-header %S/Inputs/objc_implementation.h -emit-ir \
// RUN:   -use-clang-function-types \
// RUN:   -target %target-future-triple > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -disable-objc-interop \
// RUN:   -F %clang-importer-sdk-path/frameworks %s \
// RUN:   -import-objc-header %S/Inputs/objc_implementation.h -emit-sil \
// RUN:   -use-clang-function-types \
// RUN:   -experimental-print-full-convention \
// RUN:   -target %target-future-triple | %FileCheck %s --check-prefix=SIL

@implementation @c
public func implFunc(_ param: Int32) {}

@implementation @c
public func implFuncCName(_ param: Int32) {}

@implementation @c(implFuncRenamed_C)
public func implFuncRenamed_Swift(param: Int32) {}

@implementation @c
public func implFuncPreserveMost(_ param: Int32) -> Int32 {
  return param
}

public func fn() {
  implFunc(2)
  implFuncCName(3)
  implFuncRenamed_Swift(param: 4)
  _ = implFuncPreserveMost(5)
}

/// implFunc(_:)
// CHECK-LABEL: define{{.*}} void @implFunc

// CHECK-NOT: define{{.*}} swiftcc void @"$s16c_implementation8implFuncyys5Int32VF"

/// inplFuncCName(_:)
// CHECK-LABEL: define{{.*}} void @"\01_implFuncAsmName"

// CHECK-NOT: define{{.*}} swiftcc void @"$s16c_implementation13implFuncCNameyys5Int32VF"

/// implFuncPreserveMost(_:)
// SIL-LABEL: sil [asmname "implFuncPreserveMost"]
// SIL-SAME: @convention(c, cType:
// SIL-SAME: preserve_most

/// fn()
// CHECK-LABEL: define{{.*}} swiftcc void @"$s16c_implementation2fnyyF"
// CHECK:   call void @implFunc
// CHECK:   call void @"\01_implFuncAsmName"
// CHECK:   call void @implFuncRenamed_C
// CHECK:   ret void
// CHECK: }

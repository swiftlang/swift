// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/main.swift -Osize -emit-ir -disable-llvm-optzns -cxx-interoperability-mode=default -import-objc-header %t/clang-records.h -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll

//--- clang-records.h

struct
    __attribute__((swift_attr("import_reference")))
    __attribute__((swift_attr("retain:retainManagedReference")))
    __attribute__((swift_attr("release:releaseManagedReference")))
    ManagedReference {};

void retainManagedReference(ManagedReference *);
void releaseManagedReference(ManagedReference *);

struct InnerClangRecord {
  ManagedReference *value;
  unsigned : 1;
};

struct OuterClangRecord {
  InnerClangRecord first;
  InnerClangRecord second;
};

//--- main.swift

class MyNontrivialType {}

public struct Inner {
  let a: MyNontrivialType
  let b: MyNontrivialType
  let c: MyNontrivialType
}

public struct Outer {
  let a: Inner
  let b: Inner
}

public func copyOuter(_ value: Outer) -> Outer {
  value
}

public struct ClangRecordWrapper {
  let first: OuterClangRecord
  let second: OuterClangRecord
}

public func copyClangRecordWrapper(
  _ value: ClangRecordWrapper
) -> ClangRecordWrapper {
  value
}

// Inner's initialize-with-copy witness must emit its implementation directly
// so that the outlined helper does not infinitely recurse

// CHECK-LABEL: define internal ptr @"$s3out5InnerVwcp"
// CHECK-NOT: call ptr @"$s{{.*}}WOc"
// CHECK: ret ptr

// Outer's initialize-with-copy witness delegates each Inner field to the
// shared outlined helper.
// CHECK-LABEL: define internal ptr @"$s3out5OuterVwcp"
// CHECK: call ptr @"$s3out5InnerVWOc"
// CHECK: call ptr @"$s3out5InnerVWOc"
// CHECK-NOT: call ptr @"$s{{.*}}WOc"
// CHECK: ret ptr

// The outer Clang record may outline operations for its imported fields.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo16OuterClangRecordVwcp"
// CHECK: call ptr @"$sSo16InnerClangRecordVWOc"
// CHECK: call ptr @"$sSo16InnerClangRecordVWOc"
// CHECK: ret ptr

// The imported reference field has a stable type and may use an outlined
// helper. The unnamed bitfield has no SILType with which to identify a helper,
// so its opaque storage must be copied directly.
// CHECK-LABEL: define linkonce_odr hidden ptr @"$sSo16InnerClangRecordVwcp"
// CHECK: call ptr @"$sSo16ManagedReferenceVSgWOc"
// CHECK: %"src.<unimported>" = getelementptr
// CHECK-NOT: call ptr @"$s{{.*}}WOc"
// CHECK: load i8, ptr %"src.<unimported>"
// CHECK: store i8
// CHECK: ret ptr

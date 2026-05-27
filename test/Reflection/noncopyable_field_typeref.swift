// Verify that swift-reflection-dump can decode field type references for
// types containing noncopyable fields.

// REQUIRES: no_asan

// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros

// rdar://100805115
// UNSUPPORTED: CPU=arm64e
// UNSUPPORTED: OS=linux-android

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %module-target-future %s -parse-as-library -emit-module -emit-library -module-name NoncopyableFields -o %t/%target-library-name(NoncopyableFields)
// RUN: %target-swift-reflection-dump %t/%target-library-name(NoncopyableFields) | %FileCheck %s

struct NC: ~Copyable {}

struct AlwaysNoncopyableBox<T>: ~Copyable {
  var value: T
}

struct ConditionallyNoncopyableBox<T: ~Copyable>: ~Copyable {
  var value: T
}
extension ConditionallyNoncopyableBox: Copyable where T: Copyable {}

struct Outer<U>: ~Copyable {
  var box: AlwaysNoncopyableBox<U>
  var condCopyableBox1: ConditionallyNoncopyableBox<Int>
  var condCopyableBox2: ConditionallyNoncopyableBox<NC>
}

// CHECK: FIELDS:
// CHECK: =======
// CHECK: NoncopyableFields.NC
// CHECK: --------------------

// CHECK: NoncopyableFields.AlwaysNoncopyableBox
// CHECK: --------------------------------------
// CHECK: value: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: NoncopyableFields.ConditionallyNoncopyableBox
// CHECK: ---------------------------------------------
// CHECK: value: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: NoncopyableFields.Outer
// CHECK: -----------------------
// CHECK: box: NoncopyableFields.AlwaysNoncopyableBox<A>
// CHECK-NEXT: (bound_generic_struct NoncopyableFields.AlwaysNoncopyableBox
// CHECK-NEXT:   (generic_type_parameter depth=0 index=0))

// CHECK: condCopyableBox1: NoncopyableFields.ConditionallyNoncopyableBox<Swift.Int>
// CHECK-NEXT: (bound_generic_struct NoncopyableFields.ConditionallyNoncopyableBox
// CHECK-NEXT:   (struct Swift.Int))

// CHECK: condCopyableBox2: NoncopyableFields.ConditionallyNoncopyableBox<NoncopyableFields.NC>
// CHECK-NEXT: (bound_generic_struct NoncopyableFields.ConditionallyNoncopyableBox
// CHECK-NEXT:   (struct NoncopyableFields.NC))

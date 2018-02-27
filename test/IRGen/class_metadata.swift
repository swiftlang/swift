// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-ptrsize

class A {}

// CHECK:      [[A_NAME:@.*]] = private constant [2 x i8] c"A\00"
// CHECK-LABEL: @"$S14class_metadata1ACMn" =
//   Flags. -2147483568 == 0x8000_0050 == HasVTable | Unique | Class
// CHECK-SAME: i32 -2147483568,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[A_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1ACMa"
//   Superclass.
// CHECK-SAME: i32 0,
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-SAME: i32 11,
//   Is reflectable.
// CHECK-SAME: i32 1,
//   V-table offset.
// CHECK-SAME: i32 10,
//   V-table length.
// CHECK-SAME: i32 1,
//   V-table entry #1: invocation function.
// CHECK-SAME: @"$S14class_metadata1ACACycfc"
//   V-table entry #1: flags.
// CHECK-SAME: i32 1 } }>, section

class B : A {}

// CHECK:      [[B_NAME:@.*]] = private constant [2 x i8] c"B\00"
// CHECK-LABEL: @"$S14class_metadata1BCMn" =
//   Flags. 80 == 0x50 == Unique | Class
// CHECK-SAME: i32 80,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[B_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1BCMa"
//   Superclass.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1ACMn"
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-SAME: i32 11,
//   Is reflectable.
// CHECK-SAME: i32 1 }>, section

class C<T> : B {}

// CHECK:      [[C_NAME:@.*]] = private constant [2 x i8] c"C\00"
// CHECK-LABEL: @"$S14class_metadata1CCMn" =
//   Flags. 208 == 0xd0 == Generic | Unique | Class
// CHECK-SAME: i32 208,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[C_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMa"
//   Superclass.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1BCMn"
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-SAME: i32 12,
//   Is reflectable.
// CHECK-SAME: i32 1,
//   Argument offset.
// CHECK-SAME: i32 11,
//   Instantiation function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMi"
//   Instantiation cache.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMI"
//   Generic parameter count.
// CHECK-SAME: i32 1,
//   Generic requirement count.
// CHECK-SAME: i32 0,
//   Key generic arguments count.
// CHECK-SAME: i32 1,
//   Extra generic arguments count.
// CHECK-SAME: i32 0,
//   Generic parameter descriptor #1: flags. -128 == 0x80 == Key
// CHECK-SAME: i8 -128,
///  Padding.
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0 }>, section

// For stupid reasons, when we declare the superclass after the subclass,
// we end up using an indirect reference to the nominal type descriptor.
class D : E {}

// CHECK:      [[D_NAME:@.*]] = private constant [2 x i8] c"D\00"
// CHECK-LABEL: @"$S14class_metadata1DCMn" =
//   Flags. 268435536 == 0x10000050 == IndirectSuperclass | Unique | Class
// CHECK-SAME: i32 268435536,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[D_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1DCMa"
//   Superclass.
// CHECK-SAME: i32 {{.*}} @"got.$S14class_metadata1ECMn
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-SAME: i32 11,
//   Is reflectable.
// CHECK-SAME: i32 1 }>, section

class E {}

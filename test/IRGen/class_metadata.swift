// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-ptrsize

class A {}

// CHECK:      [[A_NAME:@.*]] = private constant [2 x i8] c"A\00"
// CHECK-LABEL: @"$S14class_metadata1ACMn" =
//   Flags. -2147418032 == 0x8001_0050 == HasVTable | Reflectable | Unique | Class
// CHECK-SAME: i32 -2147418032,
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
// CHECK-32-SAME: i32 14,
// CHECK-64-SAME: i32 11,
//   V-table offset.
// CHECK-32-SAME: i32 13,
// CHECK-64-SAME: i32 10,
//   V-table length.
// CHECK-SAME: i32 1,
//   V-table entry #1: invocation function.
// CHECK-SAME: @"$S14class_metadata1ACACycfc"
//   V-table entry #1: flags.
// CHECK-SAME: i32 1 } }>, section

class B : A {}

// CHECK:      [[B_NAME:@.*]] = private constant [2 x i8] c"B\00"
// CHECK-LABEL: @"$S14class_metadata1BCMn" =
//   Flags. 65616 == 0x0001_0050 == Reflectable | Unique | Class
// CHECK-SAME: i32 65616,
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
// CHECK-32-SAME: i32 14 }>, section
// CHECK-64-SAME: i32 11 }>, section

class C<T> : B {}

// CHECK:      [[C_NAME:@.*]] = private constant [2 x i8] c"C\00"
// CHECK-LABEL: @"$S14class_metadata1CCMn" =
//   Flags. 65744 == 0x0001_00d0 == Reflectable | Generic | Unique | Class
// CHECK-SAME: i32 65744,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[C_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMa"
//   Superclass.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1BCMn"
//   Negative size in words.
// CHECK-SAME: i32 2,
//   Positive size in words.
// CHECK-32-SAME: i32 15,
// CHECK-64-SAME: i32 12,
//   Num immediate members.
// CHECK-32-SAME: i32 1,
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-32-SAME: i32 15,
// CHECK-64-SAME: i32 12,
//   Instantiation cache.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMI"
//   Instantiation pattern.
// CHECK-SAME: i32 {{.*}} @"$S14class_metadata1CCMP"
//   Generic parameter count.
// CHECK-SAME: i16 1,
//   Generic requirement count.
// CHECK-SAME: i16 0,
//   Key generic arguments count.
// CHECK-SAME: i16 1,
//   Extra generic arguments count.
// CHECK-SAME: i16 0,
//   Generic parameter descriptor #1: flags. -128 == 0x80 == Key
// CHECK-SAME: i8 -128,
///  Padding.
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0 }>, section

// CHECK-LABEL: @"$S14class_metadata1CCMP" =
//   Instantiation function.
// CHECK-SAME:  i32 {{.*}} @"$S14class_metadata1CCMi"

// For stupid reasons, when we declare the superclass after the subclass,
// we end up using an indirect reference to the nominal type descriptor.
class D : E {}

// CHECK:      [[D_NAME:@.*]] = private constant [2 x i8] c"D\00"
// CHECK-LABEL: @"$S14class_metadata1DCMn" =
//   Flags. 67174480 == 0x0401_0050 == Reflectable | IndirectSuperclass | Unique | Class
// CHECK-SAME: i32 67174480,
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
// CHECK-32-SAME: i32 14 }>, section
// CHECK-64-SAME: i32 11 }>, section

class E {}

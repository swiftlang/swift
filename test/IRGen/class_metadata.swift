// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/class_metadata.swift
// RUN: %target-swift-frontend  -enable-objc-interop -emit-ir %s | %FileCheck %t/class_metadata.swift -check-prefixes=CHECK,CHECK-%target-ptrsize,CHECK-%target-import-type,CHECK-%target-cpu -D#MDSIZE=7
// RUN: %target-swift-frontend -disable-objc-interop -emit-ir %s | %FileCheck %t/class_metadata.swift -check-prefixes=CHECK,CHECK-%target-ptrsize,CHECK-%target-import-type,CHECK-%target-cpu -D#MDSIZE=4
// REQUIRES: objc_codegen

class A {}

// CHECK:      [[A_NAME:@.*]] = private constant [2 x i8] c"A\00"
// CHECK-LABEL: @"$s14class_metadata1ACMn" =
//   Flags. 0x8000_0050 == HasVTable | Unique | Class
// CHECK-DIRECT-SAME: <i32 0x8000_0050>,
// CHECK-INDIRECT-SAME: <i32 0x8001_0050>,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[A_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1ACMa"
//   Superclass.
// CHECK-SAME: i32 0,
//   Negative size in words.
// CHECK-SAME: i32 3,
//   Positive size in words.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 1]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 1]],
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-32-SAME: i32 [[#MDSIZE + 6]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3]],
//   V-table offset.
// CHECK-32-SAME: i32 [[#MDSIZE + 6]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3]],
//   V-table length.
// CHECK-SAME: i32 1,
// CHECK-SAME: %swift.method_descriptor {
//   V-table entry #1: flags.
// CHECK-i386-SAME: i32 1,
// CHECK-x86_64-SAME: i32 1,
// CHECK-armv7k-SAME: i32 1,
// CHECK-arm64-SAME: i32 1,
// CHECK-arm64e-SAME: i32 1882783745
//   V-table entry #1: invocation function.
// CHECK-SAME: @"$s14class_metadata1ACACycfC"
// CHECK-SAME: }>, section

class B : A {}

// CHECK:      [[B_NAME:@.*]] = private constant [2 x i8] c"B\00"
// CHECK-LABEL: @"$s14class_metadata1BCMn" =
//   Flags. 0x4000_0050 == HasOverrideTable | Unique | Class
// CHECK-DIRECT-SAME: <i32 0x4000_0050>,
// CHECK-INDIRECT-SAME: <i32 0x4001_0050>,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[B_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1BCMa"
//   Superclass type.
// CHECK-SAME: @"symbolic _____ 14class_metadata1AC"
//   Negative size in words.
// CHECK-SAME: i32 3,
//   Positive size in words.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 1]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 1]],
//   Immediate member count.
// CHECK-SAME: i32 0,
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 1]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 1]],
//   Number of method overrides.
// CHECK-SAME: i32 1,
// CHECK-SAME: %swift.method_override_descriptor {
//   Override table entry #1: base class.
// CHECK-SAME: @"$s14class_metadata1ACMn"
//   Override table entry #1: base method.
// CHECK-DIRECT-SAME: @"$s14class_metadata1ACMn", i32 0, i32 13
// CHECK-INDIRECT-SAME: @"$s14class_metadata1ACMn", i32 0, i32 16
//   Override table entry #1: invocation function.
// CHECK-SAME: @"$s14class_metadata1BCACycfC"

// CHECK-SAME: }>, section

class C<T> : B {}

// CHECK:      [[C_NAME:@.*]] = private constant [2 x i8] c"C\00"
// CHECK-LABEL: @"$s14class_metadata1CCMn" =
//   Flags. 0x4000_00d0 == HasOverrideTable | Generic | Unique | Class
// CHECK-SAME: <i32 0x4000_00d0>,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[C_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1CCMa"
//   Superclass type.
// CHECK-SAME: @"symbolic _____ 14class_metadata1BC"
//   Negative size in words.
// CHECK-SAME: i32 3,
//   Positive size in words.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 2]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 2]],
//   Num immediate members.
// CHECK-32-SAME: i32 1,
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 2]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 2]],
//   Instantiation cache.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1CCMI"
//   Instantiation pattern.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1CCMP"
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
//   Padding.
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0,
// CHECK-SAME: i8 0,
//   Number of method overrides.
// CHECK-SAME: i32 1,
// CHECK-SAME: %swift.method_override_descriptor {
//   Override table entry #1: base class.
// CHECK-SAME: @"$s14class_metadata1ACMn"
//   Override table entry #1: base method.
// CHECK-DIRECT-SAME: @"$s14class_metadata1ACMn", i32 0, i32 13
// CHECK-INDIRECT-SAME: @"$s14class_metadata1ACMn", i32 0, i32 16
//   Override table entry #1: invocation function.
// CHECK-SAME: @"$s14class_metadata1CCACyxGycfC"
// CHECK-SAME: }>, section

// CHECK-LABEL: @"$s14class_metadata1CCMP" =
//   Instantiation function.
// CHECK-SAME:  i32 {{.*}} @"$s14class_metadata1CCMi"

// For stupid reasons, when we declare the superclass after the subclass,
// we end up using an indirect reference to the nominal type descriptor.
class D : E {}

// CHECK:      [[D_NAME:@.*]] = private constant [2 x i8] c"D\00"
// CHECK-LABEL: @"$s14class_metadata1DCMn" =
//   Flags. 0x4200_0050 == HasOverrideTable | Unique | Class
// CHECK-DIRECT-SAME: <i32 0x4000_0050>,
// CHECK-INDIRECT-SAME: <i32 0x4001_0050>,
//   Parent.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadataMXM"
//   Name.
// CHECK-SAME: i32 {{.*}} [[D_NAME]]
//   Metadata access function.
// CHECK-SAME: i32 {{.*}} @"$s14class_metadata1DCMa"
//   Superclass type.
// CHECK-SAME: @"symbolic _____ 14class_metadata1EC"
//   Negative size in words.
// CHECK-SAME: i32 3,
//   Positive size in words.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 1]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 1]],
//   Immediate member count.
// CHECK-SAME: i32 0,
//   Field count.
// CHECK-SAME: i32 0,
//   Field offset vector offset.
// CHECK-32-SAME: i32 [[#MDSIZE + 6 + 1]],
// CHECK-64-SAME: i32 [[#MDSIZE + 3 + 1]],
//   Number of method overrides.
// CHECK-SAME: i32 1,
// CHECK-SAME: %swift.method_override_descriptor {
//   Override table entry #1: base class.
// CHECK-SAME: @"$s14class_metadata1ECMn"
//   Override table entry #1: base method.
// CHECK-SAME: @"$s14class_metadata1ECMn"
//   Override table entry #1: invocation function.
// CHECK-SAME: @"$s14class_metadata1DCACycfC"
// CHECK-SAME: }>, section
class E {}

// CHECK-LABEL: @"$s14class_metadata1FCMn" =
// CHECK-SAME: @"symbolic _____yq_G 14class_metadata1CC"
class F<T, U> : C<U> { }

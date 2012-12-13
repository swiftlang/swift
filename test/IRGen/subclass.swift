// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[REF:%swift.refcounted]] = type {
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[A:%_T8subclass1A]] = type { [[REF]], %_TSs5Int64, %_TSs5Int64 }
// CHECK: [[INT:%_TSs5Int64]] = type { i64 }
// CHECK: [[FULL_HEAPMETADATA:%swift.full_heapmetadata]] = type
// CHECK: [[B:%_T8subclass1B]] = type

// CHECK: @_DATA_A = private constant {{.*\* } }}{
// CHECK: @_TMdC8subclass1A = constant [[A_METADATA:{.*\* }]] {
// CHECK:   i64 ([[REF]]*)* @_TLC8subclass1AD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0
// CHECK:   [[TYPE]]* null,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA_A to i64), i64 1),
// CHECK:   i64 ([[A]]*)* @_TC8subclass1A1ffS0_FT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TC8subclass1A1gfMS0_FT_S0_
// CHECK: }
// CHECK: @_DATA_B = private constant {{.*\* } }}{
// CHECK: @_TMdC8subclass1B = constant { {{.*}} } {
// CHECK:   i64 ([[REF]]*)* @_TLC8subclass1BD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 0
// CHECK:   [[TYPE]]* getelementptr inbounds ([[FULL_HEAPMETADATA]]* bitcast ([[A_METADATA]]* @_TMdC8subclass1A to [[FULL_HEAPMETADATA]]*), i32 0, i32 2),
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA_B to i64), i64 1),
// CHECK:   i64 ([[B]]*)* @_TC8subclass1B1ffS0_FT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TC8subclass1A1gfMS0_FT_S0_
// CHECK: }

class A {
  var x : Int
  var y : Int

  func f() -> Int { return x }
  static func g() -> A { return new A }
}

class B : A {
  func f() -> Int { return y }
}
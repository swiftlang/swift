// RUN: %target-swift-frontend -enable-objc-interop -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK-DAG: %swift.refcounted = type {
// CHECK-DAG: [[TYPE:%swift.type]] = type
// CHECK-DAG: [[OBJC_CLASS:%objc_class]] = type {
// CHECK-DAG: [[OPAQUE:%swift.opaque]] = type
// CHECK-DAG: [[A:%T8subclass1AC]] = type <{ [[REF:%swift.refcounted]], %TSi, %TSi }>
// CHECK-DAG: [[INT:%TSi]] = type <{ i64 }>
// CHECK-DAG: [[B:%T8subclass1BC]] = type <{ [[REF]], [[INT]], [[INT]], [[INT]] }>

// CHECK: @_DATA__TtC8subclass1A = private constant {{.*\* } }}{
// CHECK: @"$S8subclass1ACMf" = internal global [[A_METADATA:<{.*i64 }>]] <{
// CHECK:   void ([[A]]*)* @"$S8subclass1ACfD",
// CHECK:   i8** @"$SBoWV",
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @"$S8subclass1ACMm" to i64),
// CHECK:   [[OBJC_CLASS]]* @"OBJC_CLASS_$_{{(_TtCs12_)?}}SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1A to i64), i64 1),
// CHECK:   i64 ([[A]]*)* @"$S8subclass1AC1fSiyF",
// CHECK:   [[A]]* ([[TYPE]]*)* @"$S8subclass1AC1gACyFZ"
// CHECK: }>
// CHECK: @_DATA__TtC8subclass1B = private constant {{.*\* } }}{
// CHECK: @"$S8subclass1BCMf" = internal global <{ {{.*}} }> <{
// CHECK:   void ([[B]]*)* @"$S8subclass1BCfD",
// CHECK:   i8** @"$SBoWV",
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @"$S8subclass1BCMm" to i64),
// CHECK:   [[TYPE]]* {{.*}} @"$S8subclass1ACMf",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1B to i64), i64 1),
// CHECK:   i64 ([[B]]*)* @"$S8subclass1BC1fSiyF",
// CHECK:   [[A]]* ([[TYPE]]*)* @"$S8subclass1AC1gACyFZ"
// CHECK: }>
// CHECK: @objc_classes = internal global [2 x i8*] [i8* {{.*}} @"$S8subclass1ACN" {{.*}}, i8* {{.*}} @"$S8subclass1BCN" {{.*}}]

class A {
  var x = 0
  var y = 0

  func f() -> Int { return x }
  class func g() -> A { return A() }
  init() { }
}

class B : A {
  var z : Int = 10
  override func f() -> Int { return z }
}

class G<T> : A {
}

// Ensure that downcasts to generic types instantiate generic metadata instead
// of trying to reference global metadata. <rdar://problem/14265663>

// CHECK: define hidden swiftcc %T8subclass1GCySiG* @"$S8subclass9a_to_gint1aAA1GCySiGAA1AC_tF"(%T8subclass1AC*) {{.*}} {
func a_to_gint(a: A) -> G<Int> {
  // CHECK: call swiftcc %swift.metadata_response @"$S8subclass1GCySiGMa"(i64 0)
  // CHECK: call i8* @swift_dynamicCastClassUnconditional
  return a as! G<Int>
}
// CHECK: }

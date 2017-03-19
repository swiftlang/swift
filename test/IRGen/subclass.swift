// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: %swift.refcounted = type {
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[OBJC_CLASS:%objc_class]] = type {
// CHECK: [[OPAQUE:%swift.opaque]] = type
// CHECK: [[A:%T8subclass1AC]] = type <{ [[REF:%swift.refcounted]], %TSi, %TSi }>
// CHECK: [[INT:%TSi]] = type <{ i64 }>
// CHECK: [[B:%T8subclass1BC]] = type <{ [[REF]], [[INT]], [[INT]], [[INT]] }>

// CHECK: @_DATA__TtC8subclass1A = private constant {{.*\* } }}{
// CHECK: @_T08subclass1ACMf = internal global [[A_METADATA:<{.*i64 }>]] <{
// CHECK:   void ([[A]]*)* @_T08subclass1ACfD,
// CHECK:   i8** @_T0BoWV,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_T08subclass1ACMm to i64),
// CHECK:   [[OBJC_CLASS]]* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1A to i64), i64 1),
// CHECK:   i64 ([[A]]*)* @_T08subclass1AC1fSiyF,
// CHECK:   [[A]]* ([[TYPE]]*)* @_T08subclass1AC1gACyFZ
// CHECK: }>
// CHECK: @_DATA__TtC8subclass1B = private constant {{.*\* } }}{
// CHECK: @_T08subclass1BCMf = internal global <{ {{.*}} }> <{
// CHECK:   void ([[B]]*)* @_T08subclass1BCfD,
// CHECK:   i8** @_T0BoWV,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_T08subclass1BCMm to i64),
// CHECK:   [[TYPE]]* {{.*}} @_T08subclass1ACMf,
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1B to i64), i64 1),
// CHECK:   i64 ([[B]]*)* @_T08subclass1BC1fSiyF,
// CHECK:   [[A]]* ([[TYPE]]*)* @_T08subclass1AC1gACyFZ
// CHECK: }>
// CHECK: @objc_classes = internal global [2 x i8*] [i8* {{.*}} @_T08subclass1ACN {{.*}}, i8* {{.*}} @_T08subclass1BCN {{.*}}], section "__DATA, __objc_classlist, regular, no_dead_strip", align 8

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

// CHECK: define hidden swiftcc %T8subclass1GCySiG* @_T08subclass9a_to_gintAA1GCySiGAA1AC1a_tF(%T8subclass1AC*) {{.*}} {
func a_to_gint(a: A) -> G<Int> {
  // CHECK: call %swift.type* @_T08subclass1GCySiGMa()
  // CHECK: call i8* @swift_dynamicCastClassUnconditional
  return a as! G<Int>
}
// CHECK: }

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
// CHECK: @_TMfC8subclass1A = internal global [[A_METADATA:<{.*i64 }>]] <{
// CHECK:   void ([[A]]*)* @_TFC8subclass1AD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_TMmC8subclass1A to i64),
// CHECK:   [[OBJC_CLASS]]* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1A to i64), i64 1),
// CHECK:   i64 ([[A]]*)* @_TFC8subclass1A1ffT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TZFC8subclass1A1gfT_S0_
// CHECK: }>
// CHECK: @_DATA__TtC8subclass1B = private constant {{.*\* } }}{
// CHECK: @_TMfC8subclass1B = internal global <{ {{.*}} }> <{
// CHECK:   void ([[B]]*)* @_TFC8subclass1BD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_TMmC8subclass1B to i64),
// CHECK:   [[TYPE]]* {{.*}} @_TMfC8subclass1A,
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* null,
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1B to i64), i64 1),
// CHECK:   i64 ([[B]]*)* @_TFC8subclass1B1ffT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TZFC8subclass1A1gfT_S0_
// CHECK: }>
// CHECK: @objc_classes = internal global [2 x i8*] [i8* {{.*}} @_TMC8subclass1A {{.*}}, i8* {{.*}} @_TMC8subclass1B {{.*}}], section "__DATA, __objc_classlist, regular, no_dead_strip", align 8

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

// CHECK: define hidden swiftcc %T8subclass1GCySiG* @_TF8subclass9a_to_gintF{{.*}}(%T8subclass1AC*) {{.*}} {
func a_to_gint(a: A) -> G<Int> {
  // CHECK: call %swift.type* @_TMaGC8subclass1GSi_()
  // CHECK: call i8* @swift_dynamicCastClassUnconditional
  return a as! G<Int>
}
// CHECK: }

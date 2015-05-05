// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[OBJC_CLASS:%objc_class]] = type {
// CHECK: [[OPAQUE:%swift.opaque]] = type
// CHECK: [[A:%C8subclass1A]] = type <{ [[REF:%swift.refcounted]], %Si, %Si }>
// CHECK: [[REF]] = type {
// CHECK: [[INT:%Si]] = type <{ i64 }>
// CHECK: [[B:%C8subclass1B]] = type <{ [[REF]], [[INT]], [[INT]], [[INT]] }>
// CHECK: [[FULL_HEAPMETADATA:%swift.full_heapmetadata]] = type

// CHECK: @_DATA__TtC8subclass1A = private constant {{.*\* } }}{
// CHECK: @_TMdC8subclass1A = global [[A_METADATA:{.*i64 }]] {
// CHECK:   void ([[A]]*)* @_TFC8subclass1AD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_TMmC8subclass1A to i64),
// CHECK:   [[OBJC_CLASS]]* @"OBJC_CLASS_$_SwiftObject",
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1A to i64), i64 1),
// CHECK:   i64 ([[A]]*)* @_TFC8subclass1A1ffS0_FT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TZFC8subclass1A1gfMS0_FT_S0_
// CHECK: }
// CHECK: @_DATA__TtC8subclass1B = private constant {{.*\* } }}{
// CHECK: @_TMdC8subclass1B = global { {{.*}} } {
// CHECK:   void ([[B]]*)* @_TFC8subclass1BD,
// CHECK:   i8** @_TWVBo,
// CHECK:   i64 ptrtoint ([[OBJC_CLASS]]* @_TMmC8subclass1B to i64),
// CHECK:   [[TYPE]]* getelementptr inbounds ([[FULL_HEAPMETADATA]], [[FULL_HEAPMETADATA]]* bitcast ([[A_METADATA]]* @_TMdC8subclass1A to [[FULL_HEAPMETADATA]]*), i32 0, i32 2),
// CHECK:   [[OPAQUE]]* @_objc_empty_cache,
// CHECK:   [[OPAQUE]]* {{(@_objc_empty_vtable|null)}},
// CHECK:   i64 add (i64 ptrtoint ({ {{.*}} }* @_DATA__TtC8subclass1B to i64), i64 1),
// CHECK:   i64 ([[B]]*)* @_TFC8subclass1B1ffS0_FT_Si,
// CHECK:   [[A]]* ([[TYPE]]*)* @_TZFC8subclass1A1gfMS0_FT_S0_
// CHECK: }
// CHECK: @objc_classes = internal global [2 x i8*] [i8* {{.*}} @_TMdC8subclass1A {{.*}}, i8* {{.*}} @_TMdC8subclass1B {{.*}}], section "__DATA, __objc_classlist, regular, no_dead_strip", align 8

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

// CHECK: define hidden %C8subclass1G* @_TF8subclass9a_to_gintF{{.*}}(%C8subclass1A*) {{.*}} {
func a_to_gint(a: A) -> G<Int> {
  // CHECK: call %swift.type* @_TMaGC8subclass1GSi_()
  // CHECK: call i8* @swift_dynamicCastClassUnconditional
  return a as! G<Int>
}
// CHECK: }

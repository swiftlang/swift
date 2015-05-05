// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

var g0 : Int = 1
var g1 : (Void, Int, Void)
var g2 : (Void, Int, Int)
var g3 : Bool

// FIXME: enum IRgen
// enum TY4 { case Some(Int, Int); case None }
// var g4 : TY4

// FIXME: enum IRgen
// enum TY5 { case a(Int8, Int8, Int8, Int8)
//             case b(Int16) }
// var g5 : TY5

var g6 : Double
var g7 : Float

// FIXME: enum IRgen
// enum TY8 { case a }
// var g8 : TY8

struct TY9 { }
var g9 : TY9

// rdar://16520242
struct A {}
extension A {
  static var foo : Int = 5
}

// CHECK: [[INT:%.*]] = type <{ i64 }>
// CHECK: [[BOOL:%.*]] = type <{ i1 }>
// CHECK: [[DOUBLE:%.*]] = type <{ double }>
// CHECK: [[FLOAT:%.*]] = type <{ float }>

// CHECK-NOT: TY8
// CHECK-NOT: TY9

// CHECK: @_Tv7globals2g0Si = global [[INT]] zeroinitializer, align 8
// CHECK: @_Tv7globals2g1TT_SiT__ = global <{ [[INT]] }> zeroinitializer, align 8
// CHECK: @_Tv7globals2g2TT_SiSi_ = global <{ [[INT]], [[INT]] }> zeroinitializer, align 8
// CHECK: @_Tv7globals2g3Sb = global [[BOOL]] zeroinitializer, align 1
// CHECK: @_Tv7globals2g6Sd = global [[DOUBLE]] zeroinitializer, align 8
// CHECK: @_Tv7globals2g7Sf = global [[FLOAT]] zeroinitializer, align 4
// CHECK: @_TZvV7globals1A3fooSi = global [[INT]] zeroinitializer, align 8

// CHECK-NOT: g8
// CHECK-NOT: g9

// CHECK: define i32 @main(i32, i8**) {{.*}} {
// CHECK:      store  i64 {{.*}}, i64* getelementptr inbounds ([[INT]], [[INT]]* @_Tv7globals2g0Si, i32 0, i32 0), align 8

// FIXME: give these initializers a real mangled name
// CHECK: define internal void @globalinit_{{.*}}func0() {{.*}} {
// CHECK:      store i64 5, i64* getelementptr inbounds (%Si, %Si* @_TZvV7globals1A3fooSi, i32 0, i32 0), align 8

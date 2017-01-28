// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

var g0 : Int = 1
var g1 : (Void, Int, Void)
var g2 : (Void, Int, Int)
var g3 : Bool

// FIXME: enum IRgen
// enum TY4 { case some(Int, Int); case none }
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

// CHECK: @_T07globals2g0Siv = hidden global [[INT]] zeroinitializer, align 8
// CHECK: @_T07globals2g1yt_Siyttv = hidden global <{ [[INT]] }> zeroinitializer, align 8
// CHECK: @_T07globals2g2yt_SiSitv = hidden global <{ [[INT]], [[INT]] }> zeroinitializer, align 8
// CHECK: @_T07globals2g3Sbv = hidden global [[BOOL]] zeroinitializer, align 1
// CHECK: @_T07globals2g6Sdv = hidden global [[DOUBLE]] zeroinitializer, align 8
// CHECK: @_T07globals2g7Sfv = hidden global [[FLOAT]] zeroinitializer, align 4
// CHECK: @_T07globals1AV3fooSivZ = hidden global [[INT]] zeroinitializer, align 8

// CHECK-NOT: g8
// CHECK-NOT: g9

// CHECK: define{{( protected)?}} i32 @main(i32, i8**) {{.*}} {
// CHECK:      store  i64 {{.*}}, i64* getelementptr inbounds ([[INT]], [[INT]]* @_T07globals2g0Siv, i32 0, i32 0), align 8

// FIXME: give these initializers a real mangled name
// CHECK: define internal void @globalinit_{{.*}}func0() {{.*}} {
// CHECK:      store i64 5, i64* getelementptr inbounds (%Si, %Si* @_T07globals1AV3fooSivZ, i32 0, i32 0), align 8

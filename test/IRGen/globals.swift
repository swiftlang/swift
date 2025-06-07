// RUN: %target-swift-frontend -primary-file %s -emit-ir -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: PTRSIZE=64

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

// CHECK: @"$s7globals2g0Sivp" = hidden global [[INT]] zeroinitializer, align 8
// CHECK: @"$s7globals2g1yt_Siyttvp" = hidden global <{ [[INT]] }> zeroinitializer, align 8
// CHECK: @"$s7globals2g2yt_S2itvp" = hidden global <{ [[INT]], [[INT]] }> zeroinitializer, align 8
// CHECK: @"$s7globals2g3Sbvp" = hidden global [[BOOL]] zeroinitializer, align 1
// CHECK: @"$s7globals2g6Sdvp" = hidden global [[DOUBLE]] zeroinitializer, align 8
// CHECK: @"$s7globals2g7Sfvp" = hidden global [[FLOAT]] zeroinitializer, align 4
// CHECK: @"$s7globals1AV3fooSivpZ" = hidden global [[INT]] <{ i64 5 }>, align 8

// CHECK-NOT: g8
// CHECK-NOT: g9

// CHECK: define{{( dllexport)?}}{{( protected)?}} i32 @main(i32 %0, ptr %1) {{.*}} {
// CHECK:      store  i64 {{.*}}, ptr @"$s7globals2g0Sivp", align 8

// CHECK:  [[BUF_PROJ:%.*]] = call {{.*}} @__swift_project_value_buffer({{.*}}s7globals1gQrvp
// CHECK:  call void @llvm.memcpy{{.*}}({{.*}}[[BUF_PROJ]]


public protocol Some {}

public struct Implementer : Some {
  var w = (0, 1, 2, 3, 4)

  public init() { }
}

let g : some Some = Implementer()

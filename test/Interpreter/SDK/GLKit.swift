// RUN: %target-run-simple-swift | FileCheck %s
// FIXME: fails on iOS
// FIXME: fails in JIT mode
// REQUIRES: OS=macosx

import GLKit

let x = GLKVector4Make(1, 0, 0, 0)
let y = GLKVector4Make(0, 1, 0, 0)
let z = GLKVector4Make(0, 0, 1, 0)

println(GLKVector4DotProduct(x, y)) // CHECK: 0.0

let z2 = GLKVector4CrossProduct(x, y)
println(GLKVector4AllEqualToVector4(z, z2)) // CHECK-NEXT: true

infix operator • { precedence 150 associativity left }
infix operator ⨉ { precedence 150 }
func •(x: GLKVector4, y: GLKVector4) -> Float {
  return GLKVector4DotProduct(x, y)
}
func ⨉(x: GLKVector4, y: GLKVector4) -> GLKVector4 {
  return GLKVector4CrossProduct(x, y)
}
func ==(x: GLKVector4, y: GLKVector4) -> Bool {
  return GLKVector4AllEqualToVector4(x, y)
}

println(x • y) // CHECK-NEXT: 0.0
println(x ⨉ y == z) // CHECK-NEXT: true

let flipXY = GLKMatrix4Make(0, 1, 0, 0,
                            1, 0, 0, 0,
                            0, 0, 1, 0,
                            0, 0, 0, 1)
// FIXME: GLKMatrix4MakeWithArray takes mutable pointer arguments for no
// good reason. rdar://problem/19124355
var flipYZElements: [Float] = [1, 0, 0, 0,
                               0, 0, 1, 0,
                               0, 1, 0, 0,
                               0, 0, 0, 1]
let flipYZ = GLKMatrix4MakeWithArray(&flipYZElements)

let rotateXYZ = GLKMatrix4Multiply(flipYZ, flipXY)

let y3 = GLKMatrix4MultiplyVector4(flipXY, x)
println(y == y3) // CHECK-NEXT: true

let y4 = GLKMatrix4MultiplyVector4(flipYZ, z)
println(y == y4) // CHECK-NEXT: true

let z3 = GLKMatrix4MultiplyVector4(rotateXYZ, x)
println(z == z3) // CHECK-NEXT: true

func •(x: GLKMatrix4, y: GLKMatrix4) -> GLKMatrix4 {
  return GLKMatrix4Multiply(x, y)
}
func •(x: GLKMatrix4, y: GLKVector4) -> GLKVector4 {
  return GLKMatrix4MultiplyVector4(x, y)
}

println(y == flipXY • x) // CHECK-NEXT: true
println(x == flipXY • y) // CHECK-NEXT: true
println(z == flipXY • z) // CHECK-NEXT: true
println(x == flipYZ • x) // CHECK-NEXT: true
println(z == flipYZ • y) // CHECK-NEXT: true
println(y == flipYZ • z) // CHECK-NEXT: true
println(z == rotateXYZ • x) // CHECK-NEXT: true
println(x == rotateXYZ • y) // CHECK-NEXT: true
println(y == rotateXYZ • z) // CHECK-NEXT: true
println(z == flipYZ • flipXY • x) // CHECK-NEXT: true
println(x == flipYZ • flipXY • y) // CHECK-NEXT: true
println(y == flipYZ • flipXY • z) // CHECK-NEXT: true

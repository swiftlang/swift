// RUN: %target-run-simple-swift | FileCheck %s
// FIXME: fails on iOS
// REQUIRES: OS=macosx

import GLKit

let x = GLKVector4Make(1, 0, 0, 0)
let y = GLKVector4Make(0, 1, 0, 0)
let z = GLKVector4Make(0, 0, 1, 0)


println(GLKVector4DotProduct(x, y)) // CHECK: 0.0

let z2 = GLKVector4CrossProduct(x, y)
println(GLKVector4AllEqualToVector4(z, z2)) // CHECK-NEXT: true

infix operator • { precedence 150 }
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

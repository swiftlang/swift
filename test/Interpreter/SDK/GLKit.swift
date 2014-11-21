// RUN: %target-run-simple-swift

import GLKit

let x = GLKVector4Make(1, 0, 0, 0)
let y = GLKVector4Make(0, 1, 0, 0)
let z = GLKVector4Make(0, 0, 1, 0)

let z2 = GLKVector4CrossProduct(x, y)

println(GLKVector4DotProduct(x, y)) // CHECK: 0.0
println(GLKVector4AllEqualToVector4(z, z2)) // CHECK: true


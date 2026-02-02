// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://github.com/swiftlang/swift/issues/54018

typealias vec2 = SIMD2<Float>
struct Quadratic {
 let p0 : vec2
 let p1 : vec2
 let p2 : vec2
 init(_ p0:vec2, _ p1:vec2, _ p2:vec2) {
   self.p0 = p0
   self.p1 = p1
   self.p2 = p2
 }
 func eval(_ t:Float) -> vec2 {
  return p0 * (1.0 - t) * (1.0 - t) + (p1 * (2.0 * (1.0 - t)) + p2 * t)
 }
}

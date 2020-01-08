// RUN: not --crash %target-swift-frontend -emit-sil %s 2>&1
// REQUIRES: asserts

// NOTE: Remove `not --crash` from RUN line when TF-123 is fixed.

// FIXME(TF-123): `@differentiable` function thunking with opaque
// abstraction patterns.
func blackHole(_ x: Any) {}
let f: @differentiable (Float) -> Float = { $0 }
blackHole(f)

// FIXME(TF-123): `@differentiable` function thunking with opaque
// abstraction patterns.
struct TF_123 {
  var f: @differentiable (Float) -> Float
}
_ = \TF_123.f

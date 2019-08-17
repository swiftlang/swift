// RUN: %target-swift-frontend -emit-sil %s

func blackHole(_ x: Any) {}
let f: @differentiable (Float) -> Float = { $0 }
blackHole(f)

struct TF_123 {
  var f: @differentiable (Float) -> Float
}
_ = \TF_123.f

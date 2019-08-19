// RUN: %target-swift-frontend -emit-sil -verify %s

// TF-758: test JVP/VJP mangling + generic specialization mangling.
//
// Note: this test depends on `Array.differentiableReduce` defined in
// `stdlib/public/core/AutoDiff.swift`. The crash is not reproducible if
// `Array.differentiableReduce` is defined in this test file.
struct TF_758<Scalar> : Differentiable {}
struct TF_758_Wrapper {
  var blocks: [TF_758<Float>]

  @differentiable
  func foo(_ input: TF_758<Float>) -> TF_758<Float> {
    return blocks.differentiableReduce(input) { res, _ in res }
  }
}

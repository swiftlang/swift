// rdar://172397275
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature InferSendableFromCaptures

// REQUIRES: swift_feature_InferSendableFromCaptures

struct Rect {
  var xRange: Range<Double> { 0..<1 }
  var yRange: Range<Double> { 0..<1 }
}

func test(flag: Bool) {
  let _: KeyPath<Rect, Range<Double>> = (flag ? \Rect.xRange : \Rect.yRange) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

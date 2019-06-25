// RUN: %target-swift-frontend -typecheck -verify %s

// Test top-level functions.

// expected-note @+1 {{'linearFunc' defined here}}
func linearFunc(_ x: Float) -> Float {
  return x
}

@transposing(linearFunc, wrt: 0) // ok
func transposingLinearFunc(x: Float) -> Float {
  return x
}

// expected-error @+1 {{a derivative already exists for 'linearFunc'}}
// @differentiating(linearFunc, wrt: 0)
// func transposingLinearFuncDuplicate(x: Float) -> Float {
//   return x
// }

// expected-error @+1 {{'@transposing' attribute requires function to return a single return type or unlabeled tuple)}}
// @differentiating(linearFunc)
// func vjpSinResultWrongLabel(x: Float) -> (value: Float, pullback: (Float) -> Float) {
//   return (x, { $0 })
// }
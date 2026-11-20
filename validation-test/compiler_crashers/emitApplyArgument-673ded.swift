// {"kind":"emit-ir","signature":"emitApplyArgument(swift::irgen::IRGenFunction&, swift::CanTypeWrapper<swift::SILFunctionType>, swift::SILParameterInfo, swift::CanTypeWrapper<swift::SILFunctionType>, swift::SILParameterInfo, swift::irgen::Explosion&, swift::irgen::Explosion&)","signatureAssert":"Assertion failed: (NextValue < Values.size()), function claimNext","signatureNext":"emitPartialApplicationForwarder"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
struct a: ~Copyable {
  deinit {
  }
}
func g<h>(n: () throws -> h) rethrows -> h {
  try n()
}
func i<c>(j: c, m: Int) -> Int {
  var k = m
  let l = a()
  return g {
    l
    return k
  }
}

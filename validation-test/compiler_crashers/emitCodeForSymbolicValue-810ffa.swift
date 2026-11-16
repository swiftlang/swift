// {"kind":"emit-sil","extraArgs":["-language-mode", "5"],"signature":"(anonymous namespace)::emitCodeForSymbolicValue(swift::SymbolicValue, swift::Type, swift::SILBuilder&, swift::SILLocation&, (anonymous namespace)::StringSILInfo&)","signatureAssert":"Assertion failed: (expectedFnType->getUnsubstitutedType(module) == resultType->getUnsubstitutedType(module)), function emitCodeForSymbolicValue","signatureNext":"constantFold"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 5 %s
func a(_: () -> Int) {
}
struct b: StringInterpolationProtocol {
  var f: () -> Int
  @_semantics("constant_evaluable")
  init(literalCapacity: Int, interpolationCount: Int) {
    f = { 0 }
  }
  func appendLiteral(_: String) {
  }
  func appendInterpolation(_: Int64, r: Int) {
  }
}
struct g: ExpressibleByStringInterpolation {
  let h: b
  @_semantics("oslog.message.init_interpolation") @_semantics("constant_evaluable")
  init(stringInterpolation: b) {
    h = stringInterpolation
  }
  init(stringLiteral: String) {
  }
}
@_transparent
func i(_ m: g) {
  a(m.h.f)
}
func n(o: [Int64], p: Bool) {
  for q in o {
    if p {
      i("\(q, r: 1)")
    }
  }
}

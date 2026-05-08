// {"kind":"emit-silgen","original":"f6861216","signature":"swift::SILBuilder::createConvertFunction(swift::SILLocation, swift::SILValue, swift::SILType, bool)","signatureAssert":"Assertion failed: ((!F || opTI->isABICompatibleWith(resTI, *F).isCompatible()) && \"Can not convert in between ABI incompatible function types\"), function create","signatureNext":"SILBuilder::createUncheckedReinterpretCast"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
}
protocol b {
}
struct c: b {
  var i: a
  var d: Double
}
func e(f: String) -> (a) -> some b {
  let g =
    switch f {
    default:
      { h in
        c(i: h, d: 0.0)
      }
    }
  return g
}

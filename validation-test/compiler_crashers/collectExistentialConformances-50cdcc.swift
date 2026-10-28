// {"frontendArgs":["-emit-sil","-language-mode","6"],"kind":"custom","signature":"swift::collectExistentialConformances(swift::CanType, swift::CanType, bool)","signatureAssert":"Assertion failed: (conformance), function collectExistentialConformances","signatureNext":"Lowering::SILGenFunction::emitTransformExistential"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 %s
func c<d, e>(f: d, i: (d, () -> e) -> Bool, g: e) {
}
protocol h {
}
func bar(a: h.Type, b: h.Type) {
  c(
    f: a,
    i: {
      a == $1()
    }, g: b)
}

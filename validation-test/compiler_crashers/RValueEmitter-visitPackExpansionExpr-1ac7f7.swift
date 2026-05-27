// {"kind":"emit-silgen","original":"283c9682","signature":"(anonymous namespace)::RValueEmitter::visitPackExpansionExpr(swift::PackExpansionExpr*, swift::Lowering::SGFContext)","signatureAssert":"Assertion failed: (init && init->canPerformPackExpansionInitialization() && \"cannot emit a PackExpansionExpr without an appropriate context\"), function visitPackExpansionExpr","signatureNext":"Lowering::SILGenFunction::emitIgnoredExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
}
struct c<d: a> {
  func e<each f>(i: inout d.b, j: repeat each f) {
    func g<h>(_: h, _: inout d.b) {
    }
    try repeat g(each j, &i)
  }
}

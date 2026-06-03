// {"kind":"emit-sil","original":"73d84b46","signature":"(anonymous namespace)::SILMoveOnlyWrappedTypeEliminator::process()","signatureNext":"SILMoveOnlyWrappedTypeEliminatorPass::run"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
actor a<e> {
  func b() {
  }
}
func c(d: borrowing a<Int>) async {
  await d.b()
}

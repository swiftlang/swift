// {"kind":"emit-silgen","original":"3306f7ce","signature":"(anonymous namespace)::PatternMatchEmission::emitDestructiveCaseBlocks()::ConsumingPatternBindingVisitor::visitEnumProjection(swift::Lowering::ManagedValue, swift::EnumElementDecl*, swift::SILLocation, swift::Pattern*)","signatureNext":"Lowering::SILGenFunction::emitSwitchStmt"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
}
enum b {
}
enum c: ~Copyable {
  case d(b)
  case e
}
func f(h: consuming c) {
  switch consume h {
  case .d(let g as a):
    g
  case .e:
    0
  }
}

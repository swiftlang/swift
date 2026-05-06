// {"kind":"emit-silgen","original":"3306f7ce","signature":"swift::Lowering::SILGenFunction::emitSwitchStmt(swift::SwitchStmt*)","signatureNext":"StmtEmitter::visitBraceStmt"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a {
}
enum b: ~Copyable {
  case c
}
func d(e: consuming b) {
  switch consume e {
  case let f as a:
    f
  case .c:
    1
  }
}

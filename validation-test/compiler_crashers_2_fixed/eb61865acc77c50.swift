// {"signature":"(anonymous namespace)::SyntacticElementConstraintGenerator::visitBraceStmt(swift::BraceStmt*)"}
// RUN: not %target-swift-frontend -typecheck %s
[ switch { case (let a)print(

// {"signature":"(anonymous namespace)::SyntacticElementConstraintGenerator::visitBraceStmt(swift::BraceStmt*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
[ switch { case (let a)print(

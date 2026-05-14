// {"kind":"typecheck","original":"fae2ea2c","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"SyntacticElementSolutionApplication::visitReturnStmt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  struct b
    var c: some a = {
      if .random() {
        return b(
      }
      return d
    }(
      extension b
    {
      init?

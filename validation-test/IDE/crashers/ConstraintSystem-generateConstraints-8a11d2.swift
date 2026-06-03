// {"kind":"complete","original":"e8a6acb5","signature":"swift::constraints::ConstraintSystem::generateConstraints(swift::constraints::SyntacticElementTarget&, swift::FreeTypeVariableBinding)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::solveImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a() {
  let b =
    #^^# {
      func c(condition: @autoclosure d = e)
    }
}

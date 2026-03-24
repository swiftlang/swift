// {"kind":"complete","original":"4f6bff8c","signature":"swift::constraints::ConstraintSystem::generateConstraints(swift::constraints::SyntacticElementTarget&, swift::FreeTypeVariableBinding)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::solveForCodeCompletion"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a(
  b: @autoclosure () =
    #^^#)

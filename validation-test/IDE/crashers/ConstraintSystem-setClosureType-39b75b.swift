// {"kind":"complete","original":"2b4a2fce","signature":"swift::constraints::ConstraintSystem::setClosureType(swift::ClosureExpr const*, swift::FunctionType*)","signatureAssert":"Assertion failed: (type), function setClosureType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
a(
  #^^# {
    ( ()) in

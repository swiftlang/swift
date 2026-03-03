// {"kind":"complete","original":"119b2739","signature":"swift::constraints::ConstraintSystem::recordArgumentList(swift::constraints::ConstraintLocator*, swift::ArgumentList*)","signatureAssert":"Assertion failed: (inserted), function recordArgumentList"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
[
  switch 0 {
  case .a(#b): #^^#
  }
]

// {"kind":"complete","original":"3114c850","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (0 && \"Bad base type\"), function getContextSubstitutions","signatureNext":"TypeBase::getContextSubstitutionMap"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a<b {
  wrappedValue: b
  projectedValue: a
  init(projectedValue: )
    c(@a   = c($d: #^^#

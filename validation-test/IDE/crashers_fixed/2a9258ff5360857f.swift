// {"kind":"complete","original":"19b878fc","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureAssert":"Assertion failed: (foundVarDecl && \"VarDecl not declared in its parent?\"), function evaluate"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  guard let \a = answer
    a
    #^^#

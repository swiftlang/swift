// {"kind":"complete","original":"bae855bf","signature":"swift::GenericTypeParamDecl::getSourceRange() const","signatureAssert":"Assertion failed: (Start.isValid() == End.isValid() && \"Start and end should either both be valid or both be invalid!\"), function SourceRange"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b: c, b extension a where b #^^#

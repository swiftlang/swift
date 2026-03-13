// {"kind":"complete","original":"1867863a","signature":"swift::ExtendedNominalRequest::evaluate(swift::Evaluator&, swift::ExtensionDecl const*) const","signatureAssert":"Assertion failed: (ext->canNeverBeBound() && \"Should have been bound by bindExtensions\"), function evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
#if {
}
  extension a {
    #^^#
#elseif

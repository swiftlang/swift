// {"kind":"complete","original":"bae855bf","signature":"swift::GenericSignatureRequest::diagnoseCycle(swift::DiagnosticEngine&) const","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b: c, b extension a where b #^^#

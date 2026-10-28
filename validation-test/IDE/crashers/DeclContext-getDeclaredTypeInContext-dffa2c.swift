// {"kind":"complete","original":"7f4bcc9e","signature":"swift::DeclContext::getDeclaredTypeInContext() const","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"CompletionLookup::getAssociatedTypeType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Sequence where a == b[ {
  func c() -> #^^#
}

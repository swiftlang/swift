// {"kind":"complete","original":"ff0d9b7b","signature":"swift::GenericSignatureImpl::isValidTypeParameter(swift::Type) const","signatureNext":"LookUpConformanceInSubstitutionMap"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b> {
  associatedtype b where c: a<b>
  func d() -> #^^#
}

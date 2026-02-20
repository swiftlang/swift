// {"kind":"complete","original":"94ef21bb","signature":"swift::GenericSignatureImpl::getReducedType(swift::Type) const"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a : b
protocol c {
associatedtype d : a#^^# where d.e == f
}
struct b<g : c

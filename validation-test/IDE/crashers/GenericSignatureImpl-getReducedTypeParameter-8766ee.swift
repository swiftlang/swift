// {"kind":"complete","original":"5d4b3348","signature":"swift::GenericSignatureImpl::getReducedTypeParameter(swift::CanType) const","signatureNext":"GenericEnvironment::getOrCreateArchetypeFromInterfaceType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b > : RangeReplaceableCollection {
associatedtype b }
extension a {
struct c<Element> :
a #^^#
typealias SubSequence = c<Element>

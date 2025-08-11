// {"kind":"complete","signature":"swift::GenericSignatureImpl::getReducedType(swift::Type) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
protocol a: Collection where Iterator == b<Self> {
  struct b<c: a>: IteratorProtocol
    func d ->
    #^COMPLETE^#

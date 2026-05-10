// {"kind":"complete","original":"57e49e10","signature":"finishStorageImplInfo(swift::AbstractStorageDecl*, swift::StorageImplInfo&)","signatureNext":"StorageImplInfoRequest::evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {
lazy(b, c) { b#^^#

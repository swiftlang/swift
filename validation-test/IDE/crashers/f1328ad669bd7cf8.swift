// {"kind":"complete","signature":"Extension must have already been bound","signatureAssert":"Extension must have already been bound"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension a [ { func b {
#^COMPLETE^#

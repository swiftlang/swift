// {"kind":"complete","signature":"Extension must have already been bound"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
extension a [ { func b {
#^COMPLETE^#

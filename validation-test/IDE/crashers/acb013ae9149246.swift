// {"kind":"complete","signature":"swift::DeclContext::getASTContext() const+0xbc"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
@ a(
#^COMPLETE^# {}
let b

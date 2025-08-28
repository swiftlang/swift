// {"kind":"complete","signature":"swift::DeclContext::getASTContext() const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@ a(
#^COMPLETE^# {}
let b

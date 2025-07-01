// {"kind":"complete","signature":"swift::AnyFunctionType::getExtInfo() const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
func a(b) a#^COMPLETE^#

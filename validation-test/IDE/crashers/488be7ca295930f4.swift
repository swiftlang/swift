// {"kind":"complete","signature":"doPrintTypeInterface(swift::CompilerInvocation const&, llvm::StringRef, llvm::StringRef)"}
// Actual signature: checkSingleOverride
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
class a { subscript(a) a { set } class b : a { override subscript(a) a {
#^COMPLETE^#

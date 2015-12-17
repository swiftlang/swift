// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
class l{func a=let A{class Q{var _=a={#^A^#
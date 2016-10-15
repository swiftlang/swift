// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
protocol P{typealias e
extension{func i(t:e=[{func#^A^#
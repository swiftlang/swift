// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
#^A^#{"
protocol c{
func a
typealias b:c
typealias e:c
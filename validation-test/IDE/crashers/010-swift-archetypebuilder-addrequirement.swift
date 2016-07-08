// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
#^A^#{"
protocol c{
func a
associatedtype b:c
associatedtype e:c

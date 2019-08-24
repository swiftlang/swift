// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
#^A^#{"
protocol c{
func a
associatedtype b:c
associatedtype e:c

// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol A{protocol A{
typealias e:a
func a<T:e
enum b{func a{#^A^#
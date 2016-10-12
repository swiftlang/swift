// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
protocol B
let a{
protocol A{extension{
func b{func a:B
extension{var _=a{#^A^#
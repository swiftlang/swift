// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
protocol a{
func g:P
protocol P{
class B<T
{struct c{
#^A^#
protocol b{typealias e:B<T>
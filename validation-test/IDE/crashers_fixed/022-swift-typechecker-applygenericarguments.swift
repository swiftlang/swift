// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol a{
func g:P
protocol P{
class B<T
{struct c{
#^A^#
protocol b{associatedtype e:B<T>

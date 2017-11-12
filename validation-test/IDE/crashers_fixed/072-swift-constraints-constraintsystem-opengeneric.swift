// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
e{protocol c{
class A{let a{
let a=B
struct B<T{let b=#^A^#

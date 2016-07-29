// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
class B<T{
class A
class B:A{func b{#^A^#

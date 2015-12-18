// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
{class T{protocol b{#^A^#typealias b:B<T>struct B<b

// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
struct B<T{protocol P{let:B<T>struct B<e#^A^#
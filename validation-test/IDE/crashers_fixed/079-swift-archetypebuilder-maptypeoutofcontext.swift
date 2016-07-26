// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
struct B<T{class B<T:B<T>n#^A^#

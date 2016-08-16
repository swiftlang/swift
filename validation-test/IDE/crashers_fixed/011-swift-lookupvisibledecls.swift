// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
enum S<T where g:A{struct T enum S:#^A^#

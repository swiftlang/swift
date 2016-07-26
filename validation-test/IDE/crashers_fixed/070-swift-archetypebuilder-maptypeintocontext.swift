// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
a{func a<U{class B<T{}{B
#^A^#

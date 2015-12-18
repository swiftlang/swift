// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
protocol c{struct Q<g{var d{{class B<T:e#^A^#

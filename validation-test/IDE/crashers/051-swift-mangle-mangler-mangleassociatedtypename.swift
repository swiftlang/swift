// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
var b{A{func f<T{let H:T.c{struct S<where#^A^#

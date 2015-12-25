// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
struct A<H:A.c{protocol c func b
var _=b}#^A^#
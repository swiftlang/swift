// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
struct A<H:A.c{protocol c func b
var _=b}#^A^#

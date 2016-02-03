// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol e:A.a
class A{
protocol c:e
func a<H:A.c func a#^A^#

// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
enum b:a{var f={static#^A^#
// FIXME: we shouldn't back the parser to the beginning of the line, it leads
// to ridiculous override completions like this.
// CHECK-NOT: Decl{{.*}}/Super

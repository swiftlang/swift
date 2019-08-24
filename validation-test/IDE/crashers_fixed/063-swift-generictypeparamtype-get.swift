// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
{protocol A{func b
}enum B:A{let s=b
let A{#^A^#
// CHECK: Begin completions

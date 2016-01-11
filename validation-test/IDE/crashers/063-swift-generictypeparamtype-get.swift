// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
{protocol A{func b
}enum B:A{let s=b
let A{#^A^#
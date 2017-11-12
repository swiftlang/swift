// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
func b<T{#^A^#let t:T

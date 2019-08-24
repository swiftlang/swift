// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol A{enum B<T{case func P{#^A^#

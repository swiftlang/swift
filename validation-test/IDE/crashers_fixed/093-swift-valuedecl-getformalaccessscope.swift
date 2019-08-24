// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
func<{#^A^#protocol A{func<struct B
struct d<T where B:T>:A

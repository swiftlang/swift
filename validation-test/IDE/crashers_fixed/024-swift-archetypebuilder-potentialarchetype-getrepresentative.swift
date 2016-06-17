// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol c{func a:P
protocol P{#^A^#func a:b
associatedtype b:a

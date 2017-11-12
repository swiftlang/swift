// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
protocol A{typealias e}extension A{typealias e:e l#^A^#
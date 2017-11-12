// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
class#^A^#{
protocol e:A
protocol A:A
protocol c:e

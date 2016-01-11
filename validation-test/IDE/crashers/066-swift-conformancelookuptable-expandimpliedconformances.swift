// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
class#^A^#{
protocol e:A
protocol A:A
protocol c:e
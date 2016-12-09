// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
let a{s{func b<T{protocol P{let t:T
#^A^#

// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
:#^A^#func a
protocol a{class A:a
typealias e:A.e
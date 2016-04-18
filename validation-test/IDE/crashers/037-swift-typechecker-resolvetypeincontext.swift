// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

// REQUIRES: asserts

A{extension{
class A{func c
var d=c let t{#^A^#

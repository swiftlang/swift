// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

func searchTest(format: ()->String = { "" }#^A^#) {}

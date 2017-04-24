// RUN: not --crash %target-swift-ide-test -swift-version 3 -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

Integer#^A^#

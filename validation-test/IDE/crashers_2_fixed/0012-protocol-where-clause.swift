// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

protocol a where #^A^#

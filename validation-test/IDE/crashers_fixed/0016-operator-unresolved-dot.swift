// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

struct a { func b(c d: a) { b(c: d) == .#^A^#

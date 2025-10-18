// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts

func test(str: String?) {
  _ = str == nil #^A^#
}

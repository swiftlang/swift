// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

extension Integer {
  init() {
    self = #^A^#
  }
}

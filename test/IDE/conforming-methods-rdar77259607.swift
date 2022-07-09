// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=CC

protocol MyView {}

extension MyView {
  func foo<Content>() -> Content? {
    return nil#^CC^#
  }
}

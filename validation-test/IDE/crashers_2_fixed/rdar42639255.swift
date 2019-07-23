// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

class Foo {
  fileprivate var deltaY: Float = 0

  func foo() {
    var values: [Any]?
    values = [0, deltaY, -deltaY] #^A^#
  }
}


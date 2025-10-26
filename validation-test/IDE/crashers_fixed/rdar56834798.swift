// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B -source-filename=%s

protocol Proto {}

struct S<T: Proto> {
  typealias Value = T

  func test(arg: Int) -> Value {
    return #^A^#
  }
}

class C: Proto {
  init() {}
}
extension Proto {
  typealias Nested = C
}
func receiver<T: Proto>(arg: T) {}
func test() {
  receiver(arg: .#^B^#)
}

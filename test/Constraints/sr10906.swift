// RUN: %target-typecheck-verify-swift

protocol ViewDataSource: class {
  func foo<T>() -> [T]
}

class View {
  weak var delegate: ViewDataSource?
}

final class ViewController<T> {
  let view = View()
  init() {
    view.delegate = self
    // expected-error@-1 {{generic class 'ViewController' requires the types 'T' and 'String' be equivalent}}
  }
}

extension ViewController: ViewDataSource where T == String {
// expected-note@-1 {{requirement from conditional conformance of 'ViewController<T>' to 'ViewDataSource'}}
  func foo<T>() -> [T] {
    return []
  }
}

// RUN: %target-swift-emit-silgen %s

class AA {
  subscript<T>(_: T.Type) -> T? {
    get { fatalError() }
    set {}
  }
}

class C {
  typealias A = AA

  func f() {
    let a = AA()
    guard let result = a[Self.A.self] else { return }
    _ = result
  }
}

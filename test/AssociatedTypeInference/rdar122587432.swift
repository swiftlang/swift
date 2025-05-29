// RUN: %target-typecheck-verify-swift

public struct S<Element: Hashable> {
  public typealias A = [Element: Int]
}

extension S: Sequence {
  public func makeIterator() -> A.Iterator {
    fatalError()
  }
}

let x: (key: String, value: Int).Type = S<String>.Element.self
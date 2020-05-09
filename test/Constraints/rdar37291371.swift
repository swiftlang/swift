// RUN: %target-typecheck-verify-swift

extension Collection where Element: Numeric {
  var v: Element {
    return self.reduce(0, +)
  }
}

struct R<T> {}
func ==<T: Equatable>(lhs: R<T>, rhs: T?) {}

func foo<T>(_ e: @autoclosure @escaping () throws -> T?) -> R<T> {
  return R<T>()
}

func bar<T>(_ e: T?) -> R<T> {
  return R<T>()
}

foo([Double(1.0)].v) == Double(1.0)
bar([Double(1.0)].v) == Double(1.0)

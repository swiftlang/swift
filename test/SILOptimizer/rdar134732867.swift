// RUN: %target-swift-frontend -emit-sil -O %s

public class Base<T> {
  @inline(never)
  func f<E>(_: E, _ t: T, _ elt: T.Element) -> Bool where T: Sequence, T.Element: Equatable {
    var iter = t.makeIterator()
    return iter.next()! == elt
  }
}

public func caller(b: Base<Array<Int>>) -> Bool {
  return b.f(123, [1, 2, 3], 5)
}

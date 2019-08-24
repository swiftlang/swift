// RUN: %target-swift-frontend %s -emit-ir

extension Collection where Self.Iterator.Element: Equatable {
  func count<T: Equatable>(of element: T) -> Int where T == Self.Iterator.Element {
    return self.reduce(0) {
      sum, e in
      let isSame: Int = (e == element ? 1 : 0)
      return sum + isSame
    }
  }
}

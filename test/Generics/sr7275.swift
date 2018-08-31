// RUN: %target-typecheck-verify-swift -swift-version 4

extension Sequence where Element: AnyObject {
  public func f1(to object: AnyObject) -> Bool {
    return contains { $0 === object }
  }
}

extension Sequence where Iterator.Element: AnyObject {
  public func f2(to object: AnyObject) -> Bool {
    return contains { $0 === object }
  }
}

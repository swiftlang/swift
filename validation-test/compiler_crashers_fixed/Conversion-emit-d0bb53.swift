// RUN: %target-swift-frontend -emit-silgen %s
extension Array {
  typealias a = (Element) -> Bool
  func b(c: borrowing a) {
    filter(c)
  }
}

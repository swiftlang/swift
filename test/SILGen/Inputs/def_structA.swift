public struct A {
  public var a : Int
  public init() {
    a = 3
  }
}
extension A {
  public mutating func test() {
    a = 0
  }
}

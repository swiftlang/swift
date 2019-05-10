import itermod

public protocol MySequence {
  associatedtype Element
  associatedtype Iterator: MyIter where Iterator.Element == Element
}

public struct MyBar: MySequence, MyIter {
  public func callme() {}

  public typealias Element = UInt8
  public typealias Iterator = MyBar
}

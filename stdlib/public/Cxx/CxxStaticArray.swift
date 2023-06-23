public protocol _UnsafeCxxStaticArray: RandomAccessCollection {
  override associatedtype Iterator = IndexingIterator<Self>
  override associatedtype Index = Int
  override associatedtype Indices = Range<Int>
  override associatedtype SubSequence = Slice<Self>

  func __dataUnsafe() -> UnsafePointer<Element>
  mutating func getCount() -> Int
}

extension _UnsafeCxxStaticArray {
  public var startIndex: Int { 0 }
  public var endIndex: Int { 8 }
  
  public subscript(_ i: Int) -> Element {
    _read {
      var this = self
      let element = UnsafeRawPointer(&this)!
        .withMemoryRebound(to: Element.self, capacity: count) { $0[i] }
      yield element
    }
    _modify {
      var ptr = UnsafeMutableRawPointer(&self)!
        .withMemoryRebound(to: Element.self, capacity: count) { $0 }
      yield &ptr[i]
    }
  }
  
  public func withInnerStorage<Result>(
    _ body: (UnsafePointer<Element>) throws -> Result
  ) rethrows -> Result {
    try body(__dataUnsafe())
  }
}

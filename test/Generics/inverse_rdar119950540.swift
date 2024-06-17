// RUN: %target-swift-frontend %s -emit-silgen > /dev/null



public protocol MyIteratorProtocol<Element> {
  associatedtype Element
  mutating func next() -> Element?
}

// UnsafeBufferPointer
public struct UBP<Element> {}

public protocol MySequence<Element> {
  associatedtype Element

  associatedtype Iterator: MyIteratorProtocol where Iterator.Element == Element

  __consuming func makeIterator() -> Iterator

  func _customContainsEquatableElement(
    _ element: Element
  ) -> Bool?

  func withContiguousStorageIfAvailable<R>(
    _ body: (_ buffer: UBP<Element>) throws -> R
  ) rethrows -> R?
}

extension MySequence where Self.Iterator == Self {
  public __consuming func makeIterator() -> Iterator {
    return self
  }
}

extension MySequence {
  public func _customContainsEquatableElement(
    _ element: Iterator.Element
  ) -> Bool? {
    return nil
  }

  public func withContiguousStorageIfAvailable<R>(
    _ body: (UBP<Element>) throws -> R
  ) rethrows -> R? {
    return nil
  }
}


public struct MyIndexingIterator<Elements: MyCollection> {
}

extension MyIndexingIterator: MyIteratorProtocol, MySequence {
  public typealias Element = Elements.Element
  public typealias Iterator = MyIndexingIterator<Elements>
  public typealias SubSequence = MySequence<Element>

  public mutating func next() -> Elements.Element? {
    return nil
  }
}

public struct MyDefaultIndices<Elements: MyCollection> {}
extension MyDefaultIndices: MyCollection {
  public typealias Index = Elements.Index
  public typealias Element = Elements.Index
  public typealias Indices = MyDefaultIndices<Elements>
  public typealias SubSequence = MyDefaultIndices<Elements>
  public typealias Iterator = MyIndexingIterator<MyDefaultIndices<Elements>>

  public __consuming func makeIterator() -> Iterator { fatalError("todo") }
}

public struct MySlice<Base: MyCollection> {

}

extension MySlice: MyCollection {
  public typealias Index = Base.Index
  public typealias Indices = Base.Indices
  public typealias Element = Base.Element
  public typealias SubSequence = MySlice<Base>
  public typealias Iterator = MyIndexingIterator<MySlice<Base>>

  public __consuming func makeIterator() -> Iterator { fatalError("todo") }
}

public protocol MyCollection<Element>: MySequence {
  override associatedtype Element
  associatedtype Index /* : Comparable */

  associatedtype Iterator = MyIndexingIterator<Self>

  associatedtype SubSequence: MyCollection = MySlice<Self>
  where SubSequence.Index == Index,
        Element == SubSequence.Element,
        SubSequence.SubSequence == SubSequence

  associatedtype Indices: MyCollection = MyDefaultIndices<Self>
    where Indices.Element == Index,
          Indices.Index == Index,
          Indices.SubSequence == Indices
}

public struct MyRange<Bound> {}
extension MyRange: MySequence {
  public typealias Element = Bound
  public typealias Iterator = MyIndexingIterator<MyRange<Bound>>

  public __consuming func makeIterator() -> Iterator { fatalError("todo") }
}
extension MyRange: MyCollection {
  public typealias Index = Bound
  public typealias Indices = MyRange<Bound>
  public typealias SubSequence = MyRange<Bound>
}

public struct KVPair<Key, Value>: MyCollection {
  public typealias Element = (key: Key, value: Value)
  public typealias Index = Int
  public typealias Indices = MyRange<Int>
  public typealias SubSequence = MySlice<KVPair>

  public typealias Iterator = MyIndexingIterator<Self>
  public __consuming func makeIterator() -> Iterator { fatalError("todo") }
}

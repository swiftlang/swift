public protocol BorrowingIter<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable

 // NOTE: Span has availability that blocks us from testing it here
  mutating func nextThing() -> Element?
}

public protocol Iterable {
  associatedtype Element
  mutating func next() -> Element?
}

public struct LibraryLegacyIter: Iterable {
  public typealias Element = String
  public func next() -> String? { return "LibraryLegacyIter" }
}

public struct LibraryConformer: Seq {
  public typealias Element = String
  public typealias Iterator = LibraryLegacyIter
  public func makeIterator() -> Iterator {
    print("LibraryConformer.makeIterator()")
    return Iterator()
  }
}

#if BEFORE
public protocol Seq<Element> {
  associatedtype Element
  associatedtype Iterator: Iterable
  func makeIterator() -> Iterator
}
#else
public protocol Seq<Element> {
  associatedtype Element
  associatedtype Iterator: Iterable
  func makeIterator() -> Iterator

  associatedtype BorrowingSeqIter: BorrowingIter<Element>, ~Copyable, ~Escapable = Adapter<Iterator>
}

@reparentable
public protocol BorrowingSeq<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable
  associatedtype BorrowingSeqIter: BorrowingIter<Element>, ~Copyable, ~Escapable

  @_lifetime(borrow self)
  func makeBorrowingSeqIter() -> BorrowingSeqIter
}

extension Seq : @reparented BorrowingSeq
    where BorrowingSeqIter == Adapter<Iterator> {
  public func makeBorrowingSeqIter() -> BorrowingSeqIter {
    return Adapter(makeIterator())
  }
}

public struct Adapter<Iterator: Iterable>: BorrowingIter {
  public typealias Element = Iterator.Element
  var iterator: Iterator
  public init(_ iterator: Iterator) { self.iterator = iterator }

  public mutating func nextThing() -> Element? {
    print("Adapter invoking iterator.next()")
    return iterator.next()!
  }
}

public struct LibraryNewIter: BorrowingIter {
  public func nextThing() -> String? {
    return "LibraryNewIter"
  }
}

extension LibraryConformer {
  public typealias BorrowingSeqIter = LibraryNewIter
  public func makeBorrowingSeqIter() -> BorrowingSeqIter {
    print("LibraryConformer.makeBorrowingSeqIter()")
    return BorrowingSeqIter()
  }
}
#endif

public func libraryTest(_ x: some Seq) {
  var iter = x.makeIterator()
  print(iter.next()!)
#if !BEFORE
  libraryTestNew(x)
#endif
}

#if !BEFORE
public func libraryTestNew(_ x: some BorrowingSeq) {
  var borrowingIter = x.makeBorrowingSeqIter()
  let answer = borrowingIter.nextThing()!
  print(answer)
}
#endif

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

  var underestimatedCount: Int { get }
  func _customContainsEquality(_ e: Element) -> Bool?
}
#else
public protocol Seq<Element>: BorrowingSeq {
  associatedtype Element
  associatedtype Iterator: Iterable
  func makeIterator() -> Iterator

  associatedtype BorrowingSeqIter: BorrowingIter<Element>, ~Copyable, ~Escapable = Adapter<Iterator>

  var underestimatedCount: Int { get }
  func _customContainsEquality(_ e: Element) -> Bool?
}

@reparentable
public protocol BorrowingSeq<Element>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable
  associatedtype BorrowingSeqIter: BorrowingIter<Element>, ~Copyable, ~Escapable

  @_lifetime(borrow self)
  func makeBorrowingSeqIter() -> BorrowingSeqIter

  var underestimatedCount: Int { get }
  func _customContainsEquality(_ e: borrowing Element) -> Bool?
}

extension BorrowingSeq {
  public var underestimatedCount: Int { 1 }
  public func _customContainsEquality(_ e: borrowing Element) -> Bool? { nil }
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

extension Seq {
  public var underestimatedCount: Int { 0 }
  public func _customContainsEquality(_ e: Element) -> Bool? { nil }
}

public func libraryTest(_ x: some Seq) {
  var iter = x.makeIterator()
  print(iter.next()!)
  print("underestimatedCount = \(x.underestimatedCount)")
#if !BEFORE
  libraryTestNew(x)
#endif
}

#if !BEFORE
public func libraryTestNew(_ x: some BorrowingSeq) {
  var borrowingIter = x.makeBorrowingSeqIter()
  let answer = borrowingIter.nextThing()!
  print(answer)
  print("underestimatedCount = \(x.underestimatedCount)")
}
#endif

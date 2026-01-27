public protocol BorrowingSeqIterable {
  func borrowedElm() -> String
}

public protocol SeqIterable {
  func elm() -> String
}

public struct LibraryLegacyIter: SeqIterable {
  public func elm() -> String { "LibraryLegacyIter" }
}

public struct LibraryConformer: Seq {
  public typealias Iterator = LibraryLegacyIter
  public func makeIterator() -> Iterator {
    print("LibraryConformer.makeIterator()")
    return Iterator()
  }
}

#if BEFORE
public protocol Seq {
  associatedtype Iterator: SeqIterable
  func makeIterator() -> Iterator
}
#else
@reparentable
public protocol BorrowingSeq {
  associatedtype BorrowingSeqIter: BorrowingSeqIterable
  func makeBorrowingSeqIter() -> BorrowingSeqIter
}

public protocol Seq {
  associatedtype Iterator: SeqIterable
  func makeIterator() -> Iterator
}

// NOTE: All three seemingly redundant pieces are actually needed right now,
//   until this mess is simplified!
//
//  - The _Default_ + @_implements is needed to provide the adapter as a
//    default type witness to types like a struct conforming to Seq that didn't
//    provide their own witness for the BorrowingSeqIter requirement.
//
//  - The `typealias BorrowingSeq` is needed for conformance checking of
//    `some Seq` : BorrowingSeq to serve as *that* conformance's witness.
//
//  - The `where` clause is currently preventing TypeAliasRequirementsRequest
//    from pinning BorrowingSeqIter to the Adapter for all conformers of Seq,
//    as that request will bail-out if it sees any conditional requirements
//    on the extension.
extension Seq : @reparented BorrowingSeq where BorrowingSeqIter == Adapter<Iterator> {
  @_implements(BorrowingSeq, BorrowingSeqIter)
  public typealias _Default_BorrowingSeqIter = Adapter<Iterator>
  public typealias BorrowingSeqIter = Adapter<Iterator>

  public func makeBorrowingSeqIter() -> BorrowingSeqIter {
    return Adapter(makeIterator())
  }
}

public struct Adapter<Iterator: SeqIterable>: BorrowingSeqIterable {
  let iterator: Iterator
  public init(_ iterator: Iterator) { self.iterator = iterator }
  public func borrowedElm() -> String { return "Adapter(" + iterator.elm() + ")" }
}

public struct LibraryNewIter: BorrowingSeqIterable {
  public func borrowedElm() -> String { "LibraryNewIter" }
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
  print(x.makeIterator().elm())
#if !BEFORE
  libraryTestNew(x)
#endif
}

#if !BEFORE
public func libraryTestNew(_ x: some BorrowingSeq) {
  let borrowingIter = x.makeBorrowingSeqIter()
  let answer = borrowingIter.borrowedElm()
  print(answer)
}
#endif

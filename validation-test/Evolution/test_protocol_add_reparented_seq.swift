// RUN: %target-resilience-test --skip-application-back-deploy \
// RUN:     --experimental-features 'Reparenting,SuppressedAssociatedTypesWithDefaults,Lifetimes,LifetimeDependence'

// REQUIRES: executable_test

// REQUIRES: swift_feature_Reparenting
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_LifetimeDependence

import StdlibUnittest
import protocol_add_reparented_seq

public struct LegacyIter: Iterable {
  public typealias Element = String
  public func next() -> String? { "Client's LegacyIter" }
}

struct LegacyClientConformer: Seq {
  typealias Element = String
  typealias Iterator = LegacyIter
  func makeIterator() -> Iterator {
    print("LegacyClientConformer.makeIterator()")
    return Iterator()
  }
}

#if !BEFORE
public struct FreshIter: BorrowingIter {
  public func nextThing() -> String? { return "Client's FreshIter" }
}

struct BorrowingSeqAwareConformer: Seq {
  typealias Element = String
  typealias Iterator = LegacyIter
  public func makeIterator() -> Iterator {
    print("BorrowingSeqAwareConformer.makeIterator()")
    return Iterator()
  }

  typealias BorrowingSeqIter = FreshIter
  public func makeBorrowingSeqIter() -> BorrowingSeqIter {
    print("BorrowingSeqAwareConformer.makeBorrowingSeqIter()")
    return BorrowingSeqIter()
  }
}
#endif



var ProtocolAddReparentedTestSeq = TestSuite("ProtocolAddReparentedSeq")

// FIXME: would be better if this was validating that we invoked the
//   correct witnesses, etc, rather than just seeing if it crashes.
//   Could verify things by having the Element type be an Enum that
//   encodes the same purpose (including nesting) that the String does.
ProtocolAddReparentedTestSeq.test("Does Not Crash") {
  libraryTest(LegacyClientConformer())
  #if !BEFORE
  print("-----")
  libraryTest(BorrowingSeqAwareConformer())
  #endif
}

// FIXME: add a "Collection" protocol under Seq in the library to test that.
//  Also test a protocol on client side that extends Seq too.

runAllTests()

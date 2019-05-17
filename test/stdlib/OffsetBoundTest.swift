// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var OffsetBoundTests = TestSuite("OffsetBoundTests")

OffsetBoundTests.test("Concrete Sanity Checks") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }

  // Just some ad-hoc example sanity checks. More exhaustive things below
  let array = [1,2,3,4,5,6,7,8,9,10]
  let sliceStart = 5

  let slice = array[sliceStart...]

  expectEqual(slice.first, slice[.first])
  expectEqual(slice[sliceStart], slice[.first])
  expectEqual(slice.last, slice[.last])
  expectEqual(slice[9], slice[.last])

  let str = "abcdefghijklmnopqrstuvwxyz"
  expectEqual("def", str[.start + 3 ..< .start + 6])
  expectEqual("defghijklmnopqrstuvw", str[.start + 3 ..< .end - 3])
  expectEqual("", str[.start + 3 ..< .end - 23])
  expectEqual("z", str[.last])
  expectNil(str[.start + 26])

  expectEqual(str.first, str[.first])
  expectEqual(str.last, str[.last])

  // Range<Int> indices are the ints themselves
  expectEqual(5..<10, (3..<10)[5...])
  expectEqual(8..<10, (3..<10)[(.start + 5)...])
}

OffsetBoundTests.test("Quick Nil Out of Bounds") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  let count = 5
  let numberArray = [1,2,3,4,5]
  let numberSet = [1,2,3,4,5] as Set<Int>
  expectEqual(numberArray.startIndex, numberArray.index(at: .end - count))
  expectEqual(numberArray.endIndex, numberArray.index(at: .start + count))
  expectEqual(numberSet.startIndex, numberSet.index(at: .end - count))
  expectEqual(numberSet.endIndex, numberSet.index(at: .start + count))

  expectNil(numberArray[.start - 1])
  expectNil(numberArray[.end + 1])
  expectNil(numberArray[.end])
  expectNil(numberArray[.end - (count+1)])
  expectNil(numberArray[.start + (count)])

  expectNil(numberSet[.start - 1])
  expectNil(numberSet[.end + 1])
  expectNil(numberSet[.end])
  expectNil(numberSet[.end - (count+1)])
  expectNil(numberSet[.start + (count)])
}

// More extensive programmatic tests

OffsetBoundTests.test("Comparison") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }

  // Sanity check ordering
  for i in 0..<10 {
    expectTrue(.start + i < .end - i)
    expectTrue(.end - i > .start + i)

    for j in 0..<10 {
      if i == j {
        expectEqual(.start + i, .start + j)
        expectEqual(.end - i, .end - j)
      } else {
        expectNotEqual(.start + i, .start + j)
        expectNotEqual(.end - i, .end - j)

        if i < j {
          expectTrue(.start + i < .start + j)
          expectTrue(.end - i > .end - j)
        } else {
          expectTrue(.start + i > .start + j)
          expectTrue(.end - i < .end - j)
        }
      }
    }
  }
}

// Use subclassing to work around lack of universal qualifier
protocol TestHost {
  func checkCollection<C: Collection>(_: C) where C.Element: Equatable
}

// Apply a host's methods to various collection types
func runChecks(_ host: TestHost) {
  let numArray = [1,2,3,4,5]
  let string = "a🤓b🧟‍♀️cde\u{301}fg👻😃😎"
  let intMaxArray = [Int.max]
  let emptyOptArray = [] as Array<Optional<Bool>>
  let emptyString = ""
  let smallString = "abcdef"
  let scalars = "a🤓b🧟‍♀️cde\u{301}fg👻😃😎".unicodeScalars
  let set1 = [1.0, 2.25, 3.0, 3.5, 100.0] as Set<Double>
  let set2: Set<Double> = {
    var set2 = set1
    set2.insert(9.0)
    set2.reserveCapacity(set2.capacity * 16) // Force different salt
    return set2
  }()

  host.checkCollection(numArray)
  host.checkCollection(intMaxArray)
  host.checkCollection(emptyOptArray)
  host.checkCollection(emptyString)
  host.checkCollection(smallString)
  host.checkCollection(string)
  host.checkCollection(scalars)
  host.checkCollection(set1)
  host.checkCollection(set2)
}


OffsetBoundTests.test("Index and element fetch") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  struct IndexHost: TestHost {
    func checkCollection<C: Collection>(
      _ c: C
    ) where C.Element: Equatable {
      let count = c.count

      expectEqualSequence(c.indices,
        (0..<count).map { c.index(at: .start + $0)! })
      expectEqualSequence(c,
        (0..<count).map { c[.start + $0]! })
      expectEqualSequence(c.indices.reversed(),
        (0..<count).map { c.index(at: .last - $0)! })
      expectEqualSequence(c.reversed(),
        (0..<count).map { c[.last - $0]! })

      expectEqual(c.index(at: .last), c.index(at: .end - 1))
      expectEqual(c.startIndex, c.index(at: .end - count))
      expectEqual(c.startIndex, c.index(at: (.last - count) + 1))

      expectEqual(c.endIndex, c.index(at: .end))
      expectEqual(c.endIndex, c.index(at: .start + count))
      expectEqual(c.endIndex, c.index(at: .last + 1))
      expectEqual(c.startIndex, c.index(at: .first))

      expectEqual(c.first, c[.first])

      expectNil(c.index(at: .start + count + 1))
      expectNil(c.index(at: .end + 1))
      expectNil(c.index(at: .last + 2))
      expectNil(c[.start + count + 1])
      expectNil(c[.end + 1])
      expectNil(c[.last + 2])

      expectNil(c.index(at: .start - 1))
      expectNil(c.index(at: .end - count - 1))
      expectNil(c.index(at: .last - count))
      expectNil(c[.start - 1])
      expectNil(c[.end - count - 1])
      expectNil(c[.last - count])
    }
  }

  runChecks(IndexHost())
}

extension Collection {
  var indexRange: Range<Index> { return startIndex..<endIndex }
}

OffsetBoundTests.test("Slicing") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  struct SlicingHost: TestHost {
    func checkCollection<C: Collection>(
      _ c: C
    ) where C.Element: Equatable {
      let count = c.count
      for (i, idx) in c.indices.enumerated() {
        expectEqualSequence(c[idx...], c[(.start + i)...])
        expectEqual(c[idx..<idx].indexRange,
          c[.start + i ..< .end - count].indexRange)
        expectEqual(c[idx..<idx].indexRange,
          c[.start + i ..< .end - count - 2].indexRange)
      }
      for (i, idx) in c.indices.reversed().enumerated() {
        expectEqualSequence(c[idx...], c[(.last - i)...])
      }

      expectEqualSequence(c, c[.start ..< .end])
      expectEqualSequence(
        c[c.endIndex..<c.endIndex], c[.start + count ..< .end])
      expectEqualSequence(
        c[c.endIndex..<c.endIndex], c[.start + count + 2 ..< .end + 1])
      expectEqualSequence(c, c[.end - count ..< .end])
      expectEqualSequence(c, c[.end - count - 2 ..< .end + 2])

      expectEqualSequence(c, c[.start - 1 ..< .end + 1])
    }
  }

  runChecks(SlicingHost())
}

OffsetBoundTests.test("Int indexing") {
  // Sanity checks that offset indexing does what we want
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  func fifth<C: Collection>(_ c: C) -> C.Element? where C.Index == Int {
    guard c.count >= 5 else { return nil }
    return c[4]
  }
  func fifthOffset<C: Collection>(_ c: C) -> C.Element? where C.Index == Int {
    guard c.count >= 5 else { return nil }
    return c[.start + 4]
  }

  func fifthThroughSixth<C: Collection>(
    _ c: C
  ) -> C.SubSequence where C.Index == Int {
    return c[4..<6]
  }
  func fifthThroughSixthOffset<C: Collection>(
    _ c: C
  ) -> C.SubSequence where C.Index == Int {
    return c[.start + 4 ..< .start + 6]
  }

  let array = [1,2,3,4,5,6,7,8,9]
  expectEqual(5, fifth(array)!)
  expectEqual(5, fifth(array[2...])!)

  expectEqual(5, fifthOffset(array)!)
  expectEqual(7, fifthOffset(array[2...])!)

  expectEqualSequence([5, 6], fifthThroughSixth(array))
  expectEqualSequence([5, 6], fifthThroughSixth(array[2...]))

  expectEqualSequence([5, 6], fifthThroughSixthOffset(array))
  expectEqualSequence([7, 8], fifthThroughSixthOffset(array[2...]))
}

#if _runtime(_ObjC)
import Foundation
OffsetBoundTests.test("Data indexing") {
  // Data indexing sanity checks
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  func fifth(_ data: Data) -> UInt8? {
    guard data.count >= 5 else { return nil }
    return data[4]
  }
  func fifthOffset(_ data: Data) -> UInt8? {
    guard data.count >= 5 else { return nil }
    return data[.start + 4]
  }

  func fifthThroughSixth(_ data: Data) -> Data {
    return data[4..<6]
  }
  func fifthThroughSixthOffset(_ data: Data) -> Data {
    return data[.start + 4 ..< .start + 6]
  }

  var data = Data([1, 2, 3, 4, 5, 6, 7, 8, 9])
  expectEqual(5, fifth(data)!)
  expectEqual(5, fifthOffset(data)!)

  expectEqualSequence([5, 6], fifthThroughSixth(data))
  expectEqualSequence([5, 6], fifthThroughSixthOffset(data))

  data = data.dropFirst()
  expectEqual(5, fifth(data)!)
  expectEqual(6, fifthOffset(data)!)

  expectEqualSequence([5, 6], fifthThroughSixth(data))
  expectEqualSequence([6, 7], fifthThroughSixthOffset(data))
}
#endif // _runtime(_ObjC)


runAllTests()

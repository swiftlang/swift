// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var OffsetBoundTests = TestSuite("OffsetBoundTests")

OffsetBoundTests.test("Concrete Sanity Checks") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  // Just some ad-hoc example sanity checks. More exhaustive things below
  var array = [1,2,3,4,5,6,7,8,9,10]
  let sliceStart = 5

  let slice = array[sliceStart...]

  expectEqual(slice.first, slice[.first])
  expectEqual(slice[sliceStart], slice[.first])
  expectEqual(slice.last, slice[.last])
  expectEqual(slice[9], slice[.last])

  // Example from "Proposed Solution"
  let str = "abcdefghijklmnopqrstuvwxyz"
  expectEqual("def", str[.first + 3 ..< .first + 6])
  expectEqual("defghijklmnopqrstuvw", str[.first + 3 ..< .last - 2])
  expectEqual("", str[.first + 3 ..< .last - 22])
  expectEqual("z", str[.last])
  expectNil(str[.first + 26])
  expectEqual("wxyz", str[(.last - 3)...])

  expectEqual("y", str[str.index(at: .last - 1)!])
  expectEqual("a", str[str.index(at: .last - 25)!])
  expectEqual(nil, str.index(at: .last - 27))

  expectEqual(str.first, str[.first])
  expectEqual(str.last, str[.last])

  // Range<Int> indices are the ints themselves
  expectEqual(5..<10, (3..<10)[5...])
  expectEqual(8..<10, (3..<10)[(.first + 5)...])

  // Example from `OffsetBound` documentation
  expectEqual("z", str[.last])
  expectEqual("x", str[.last - 2])
  expectNil(str[.first + 26])
  expectEqual("def", str[.first + 3 ..< .first + 6])
  expectEqual("defghijklmnopqrstuvw", str[.first + 3 ..< .last - 2])
  array = [1,2,3,4,5,6]
  expectEqual(4, array[2...][3])
  expectEqual(6, array[2...][.first + 3]!)

  // Example from subscript documentation
  var abcs = "abcdefg"
  expectEqual("g", abcs[.last])
  expectEqual("e", abcs[.last - 2])
  expectNil(abcs[.first + 8])
  abcs[.first + 2] = "©"
  expectEqual("ab©defg", abcs)
  abcs[.last - 1] = nil
  expectEqual("ab©deg", abcs)

  abcs = "abcdefg"
  expectEqual("bcdef", abcs[.first + 1 ..< .first + 6])
  expectEqual("bcde", abcs[.first + 1 ..< .last - 1])
  abcs[.first ... .first + 3] = "🔡"
  expectEqual("🔡efg", abcs)

  // Example from replaceSubrange documentation
  var animals = "🐕🐈🐱🐩"
  let dogFaces = repeatElement("🐶" as Character, count: 3)
  animals.replaceSubrange(.first + 1 ... .last - 1, with: dogFaces)
  expectEqual("🐕🐶🐶🐶🐩", animals)

  // Example from insert documentation
  var numbers = "12345"
  numbers.insert("Ⅸ", at: .first + 1)
  numbers.insert("𐄕", at: .last + 1)
  expectEqual("1Ⅸ2345𐄕", numbers)

  numbers = "12345"
  numbers.insert(contentsOf: "↉⅖⅑", at: .first + 2)
  expectEqual("12↉⅖⅑345", numbers)

  // Example from remove documentation
  var measurements = [1.2, 1.5, 2.9, 1.2, 1.6]
  let removed = measurements.remove(at: .last - 2)
  expectEqual(2.9, removed)
  expectEqual([1.2, 1.5, 1.2, 1.6], measurements)
  expectNil(measurements.remove(at: .first + 4))

  // Example from removeSubrange documentation
  var rulers = "📏🤴👑📐"
  rulers.removeSubrange(.first + 1 ... .last - 1)
  expectEqual("📏📐", rulers)

}

OffsetBoundTests.test("Quick Nil Out of Bounds") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  let count = 5
  let numberArray = [1,2,3,4,5]
  let numberSet = [1,2,3,4,5] as Set<Int>
  expectEqual(numberArray.startIndex, numberArray.index(at: .last + 1 - count))
  expectEqual(numberArray.endIndex, numberArray.index(at: .first + count))
  expectEqual(numberSet.startIndex, numberSet.index(at: .last + 1 - count))
  expectEqual(numberSet.endIndex, numberSet.index(at: .first + count))

  expectNil(numberArray[.first - 1])
  expectNil(numberArray[.last + 2])
  expectNil(numberArray[.last + 1])
  expectNil(numberArray[.last - count])
  expectNil(numberArray[.first + (count)])

  expectNil(numberSet[.first - 1])
  expectNil(numberSet[.last + 2])
  expectNil(numberSet[.last + 1])
  expectNil(numberSet[.last - count])
  expectNil(numberSet[.first + count])
}

// More extensive programmatic tests

OffsetBoundTests.test("Comparison") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }


  // Sanity check ordering
  for i in 0..<10 {
    expectTrue(.first + i < .last + 1 - i)
    expectTrue(.last - i > .first + i)

    for j in 0..<10 {
      if i == j {
        expectEqual(.first + i, .first + j)
        expectEqual(.last - i, .last - j)
      } else {
        expectNotEqual(.first + i, .first + j)
        expectNotEqual(.last - i, .last - j)

        if i < j {
          expectTrue(.first + i < .first + j)
          expectTrue(.last - i > .last - j)
        } else {
          expectTrue(.first + i > .first + j)
          expectTrue(.last - i < .last - j)
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
        (0..<count).map { c.index(at: .first + $0)! })
      expectEqualSequence(c,
        (0..<count).map { c[.first + $0]! })
      expectEqualSequence(c.indices.reversed(),
        (0..<count).map { c.index(at: .last - $0)! })
      expectEqualSequence(c.reversed(),
        (0..<count).map { c[.last - $0]! })

      expectEqual(c.startIndex, c.index(at: .last + 1 - count))
      expectEqual(c.startIndex, c.index(at: (.last - count) + 1))

      expectEqual(c.endIndex, c.index(at: .last + 1))
      expectEqual(c.endIndex, c.index(at: .first + count))
      expectEqual(c.startIndex, c.index(at: .first))

      expectEqual(c.first, c[.first])

      expectNil(c.index(at: .first + count + 1))
      expectNil(c.index(at: .last + 2))
      expectNil(c.index(at: .last + 2))
      expectNil(c[.first + count + 1])
      expectNil(c[.last + 2])

      expectNil(c.index(at: .first - 1))
      expectNil(c.index(at: .last - count))
      expectNil(c[.first - 1])
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
        expectEqualSequence(c[idx...], c[(.first + i)...])
        expectEqual(c[idx..<idx].indexRange,
          c[.first + i ..< .last + 1 - count].indexRange)
        expectEqual(c[idx..<idx].indexRange,
          c[.first + i ..< .last - count - 1].indexRange)
      }
      for (i, idx) in c.indices.reversed().enumerated() {
        expectEqualSequence(c[idx...], c[(.last - i)...])
      }

      expectEqualSequence(c, c[.first ..< .last + 1])
      expectEqualSequence(
        c[c.endIndex..<c.endIndex], c[.first + count ..< .last + 1])
      expectEqualSequence(
        c[c.endIndex..<c.endIndex], c[.first + count + 2 ..< .last + 2])
      expectEqualSequence(c, c[.last + 1 - count ..< .last + 1])
      expectEqualSequence(c, c[.last - count - 1 ..< .last + 3])

      expectEqualSequence(c, c[.first - 1 ..< .last + 2])
    }
  }

  runChecks(SlicingHost())
}

OffsetBoundTests.test("Int indexing") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }

  // Sanity checks that offset indexing does what we want
  func fifth<C: Collection>(_ c: C) -> C.Element? where C.Index == Int {
    return c.count >= 5 ? c[4] : nil
  }
  func fifthOffset<C: Collection>(_ c: C) -> C.Element? where C.Index == Int {
    return c[.first + 4]
  }

  func fifthThroughSixth<C: Collection>(
    _ c: C
  ) -> C.SubSequence where C.Index == Int {
    return c[4..<6]
  }
  func fifthThroughSixthOffset<C: Collection>(
    _ c: C
  ) -> C.SubSequence where C.Index == Int {
    return c[.first + 4 ..< .first + 6]
  }

  expectNil(fifth([1,2,3,4]))
  expectNil(fifthOffset([1,2,3,4]))

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

OffsetBoundTests.test("RangeReplaceableCollection") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }

  var array = [0]

  func checkArray(_ expect: [Int], _ operation: @autoclosure () -> (),
    file: String = #file, line: UInt = #line
  ) {
    operation()
    expectEqual(expect, array, file: file, line: line)
  }

  checkArray([1, 0], array.insert(1, at: .last))
  checkArray([1, 0, 2], array.insert(2, at: .first + 100))
  checkArray([3, 1, 0, 2], array.insert(3, at: .last - 100))

  checkArray([4, 4, 3, 1, 0, 2], array.insert(contentsOf: [4, 4], at: .last - 100))

  expectNil(array.remove(at: .first + 100))
  do {
    let elt = array.remove(at: .first + 2)
    expectEqual(elt, 3)
  }

  checkArray([4, 4, 1, 0, 2], ())

  // Empty range is no-op
  checkArray([4, 4, 1, 0, 2], array.removeSubrange(.first + 2 ..< .last - 2))

  checkArray([4, 0, 2], array.removeSubrange(.first + 1 ..< .last - 1))
  checkArray([4, 0], array.removeSubrange(.first + 2 ..< .first + 100))

  checkArray([4, 8], array[.last] = 8)
  checkArray([4, 8, 9], array[.last + 1] = 9)
  checkArray([4, 8], array[.last] = nil)
  checkArray([4, 8], array[.last + 2] = nil)
  checkArray([4, 8, 9], array[.last + 2] = 9)

  checkArray([0, 1, 2, 8, 9], array[...(.first)] = [0, 1, 2])
  checkArray([0, 1, 2, 8, 9, 0, 1, 2], array[.last + 2 ..< .last + 3] = [0, 1, 2])
  checkArray([0, 1, 4, 2], array.replaceSubrange(.first + 2 ... .last - 1, with: [4]))
}

#if _runtime(_ObjC)
import Foundation
OffsetBoundTests.test("Data indexing") {
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return
  }
  // Data indexing sanity checks
  func fifth(_ data: Data) -> UInt8? {
    return data.count >= 5 ? data[4] : nil
  }
  func fifthOffset(_ data: Data) -> UInt8? {
    return data[.first + 4]
  }

  func fifthThroughSixth(_ data: Data) -> Data {
    return data[4..<6]
  }
  func fifthThroughSixthOffset(_ data: Data) -> Data {
    return data[.first + 4 ..< .first + 6]
  }

  expectNil(fifth(Data([1,2,3,4])))
  expectNil(fifthOffset(Data([1,2,3,4])))

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

// RUN: rm -rf %t ; mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4

// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

//===--- MyCollection -----------------------------------------------------===//
/// A simple collection that attempts to use an Int16 IndexDistance
struct MyCollection<Element>: Collection {
  var _elements: [Element]
  
  typealias IndexDistance = Int16
  typealias Index = Int16
  
  var startIndex: Index { return 0 }
  var endIndex: Index { return numericCast(_elements.count) }
  
  subscript(i: Index) -> Element { return _elements[Int(i)] }

  func index(after: Index) -> Index { return after+1 }
}

//===--- MyBidiCollection -------------------------------------------------===//
/// A simple collection that doesn't declare an IndexDistance
struct MyBidiCollection<Element>: BidirectionalCollection {
  var _elements: [Element]
  
  typealias Index = Int64
  
  var startIndex: Index { return 0 }
  var endIndex: Index { return numericCast(_elements.count) }
  
  subscript(i: Index) -> Element { return _elements[Int(i)] }

  func index(after: Index) -> Index { return after+1 }
  func index(before: Index) -> Index { return before-1 }
  func index(_ i: Index, advancedBy d: Int64) -> Index { return i+d }
}


let CollectionDistance = TestSuite("Collection.IndexDistance")

CollectionDistance.test("Int16/distance") {
  let c = MyCollection<Int>(_elements: [1,2,3])
  var d: Int16 = c.distance(from: c.startIndex, to: c.endIndex)
  expectEqual(3, d)
  expectType(MyCollection<Int>.IndexDistance.self, &d)
  // without type context, you now get an Int
  var i = c.distance(from: c.startIndex, to: c.endIndex)
  expectType(Int.self, &i)
}

CollectionDistance.test("Int16/advance") {
  let c = MyCollection<Int>(_elements: [1,2,3])
  let d: Int16 = 1
  var i = c.index(c.startIndex, offsetBy: d)
  expectEqual(1, i)
  c.formIndex(&i, offsetBy: d)
  expectEqual(2, i)
  let j = c.index(c.startIndex, offsetBy: d, limitedBy: c.endIndex)
  expectEqual(1, j)
  var b = c.formIndex(&i, offsetBy: d, limitedBy: c.endIndex)
  expectTrue(b)
  expectEqual(3, i)
  b = c.formIndex(&i, offsetBy: d+5, limitedBy: c.endIndex)
  expectFalse(b)
  expectEqual(3, i)
  let k = c.index(c.startIndex, offsetBy: d+5, limitedBy: c.endIndex)
  expectEqual(nil, k)

  checkCollection(c, [1,2,3], stackTrace: SourceLocStack()) { $0 == $1 }
}

CollectionDistance.test("Int64/distance") {
  let c = MyBidiCollection<Int>(_elements: [1,2,3])
  var d = c.distance(from: c.startIndex, to: c.endIndex)
  expectEqual(3, d)
  expectType(Int.self, &d)
  expectType(MyBidiCollection<Int>.IndexDistance.self, &d)
}

CollectionDistance.test("Int64/advance") {
  let c = MyBidiCollection<Int>(_elements: [1,2,3])
  let d: Int16 = 1
  var i = c.index(c.startIndex, offsetBy: d)
  expectEqual(1, i)
  c.formIndex(&i, offsetBy: d)
  expectEqual(2, i)
  let j = c.index(c.startIndex, offsetBy: d, limitedBy: c.endIndex)
  expectEqual(1, j)
  var b = c.formIndex(&i, offsetBy: d, limitedBy: c.endIndex)
  expectTrue(b)
  expectEqual(3, i)
  b = c.formIndex(&i, offsetBy: d+5, limitedBy: c.endIndex)
  expectFalse(b)
  expectEqual(3, i)
  let k = c.index(c.startIndex, offsetBy: d+5, limitedBy: c.endIndex)
  expectEqual(nil, k)

  checkCollection(c, [1,2,3], stackTrace: SourceLocStack()) { $0 == $1 }
  checkBidirectionalCollection(c, [1,2,3])
}

extension Collection where Index == Int, IndexDistance == Int {
  var myCount: Int {
    return distance(from: startIndex, to: endIndex)
  }
}

CollectionDistance.test("IndexDistance/constraint") {
  let n = [1,2,3].myCount
  expectEqual(3, n)
}

runAllTests()

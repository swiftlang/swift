// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)

// Also test this with a bridging header instead of the StdMap module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-map.h %t2/std-map-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-map-bridging-header.h -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-map-bridging-header.h -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-map-bridging-header.h -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test

import StdlibUnittest
#if !BRIDGING_HEADER
import StdMap
#endif
import CxxStdlib
import Cxx

var StdMapTestSuite = TestSuite("StdMap")

StdMapTestSuite.test("init") {
  let m = Map()
  expectEqual(m.size(), 0)
  expectTrue(m.empty())
}

StdMapTestSuite.test("Map.init(_: Dictionary<Int, Int>)") {
  let swiftDict: [Int32 : Int32] = [-1: 2, 2: 3, 33: 44]
  let m = Map(swiftDict)
  expectEqual(m.size(), 3)

  expectEqual(m[-1], 2)
  expectEqual(m[2], 3)
  expectEqual(m[33], 44)

  let emptySwiftDict: [Int32 : Int32] = [:]
  let emptyM = Map(emptySwiftDict)
  expectEqual(emptyM.size(), 0)
}

/// Same as above, but for std::unordered_map.
StdMapTestSuite.test("UnorderedMap.init(_: Dictionary<Int, Int>)") {
  let swiftDict: [Int32 : Int32] = [-1 : 2, 2 : 3, 33 : 44]
  let m = UnorderedMap(swiftDict)
  expectEqual(m.size(), 3)

  expectEqual(m[-1], 2)
  expectEqual(m[2], 3)
  expectEqual(m[33], 44)

  let emptySwiftDict: [Int32 : Int32] = [:]
  let emptyM = UnorderedMap(emptySwiftDict)
  expectEqual(emptyM.size(), 0)
}

StdMapTestSuite.test("MapStrings.init(_: Dictionary<std.string, std.string>)") {
  let swiftDict = [std.string("abc") : std.string("123"),
                   std.string() : std.string("empty")]
  let m = MapStrings(swiftDict)
  expectEqual(m.size(), 2)

  expectEqual(m[std.string("abc")], std.string("123"))
  expectEqual(m[std.string()], std.string("empty"))

  let emptySwiftDict: [std.string : std.string] = [:]
  let emptyM = MapStrings(emptySwiftDict)
  expectEqual(emptyM.size(), 0)
}

StdMapTestSuite.test("Map as ExpressibleByDictionaryLiteral") {
  let m: Map = [-1 : 2, 2 : 3, 33 : 44]
  expectEqual(m.size(), 3)

  func takesMap(_ m: Map) {
    expectEqual(m[-1], 2)
    expectEqual(m[2], 3)
    expectEqual(m[33], 44)
  }

  takesMap(m)
  takesMap([-1 : 2, 2 : 3, 33 : 44])
}

/// Same as above, but for std::unordered_map.
StdMapTestSuite.test("UnorderedMap as ExpressibleByDictionaryLiteral") {
  let m: UnorderedMap = [-1 : 2, 2 : 3, 33 : 44]
  expectEqual(m.size(), 3)

  func takesUnorderedMap(_ m: UnorderedMap) {
    expectEqual(m[-1], 2)
    expectEqual(m[2], 3)
    expectEqual(m[33], 44)
  }

  takesUnorderedMap(m)
  takesUnorderedMap([-1 : 2, 2 : 3, 33 : 44])
}

StdMapTestSuite.test("Map.init(grouping:by:)") {
  let m = MapGroup(
            grouping: [
                10,
                20, 21, 22,
                30, 31, 32, 33, 34
            ], by: { $0 / 10 })

  expectEqual(m.size(), 3)

  expectNil(m[0])
  expectEqual(m[1]?.size(), 1)
  expectEqual(m[2]?.size(), 3)
  expectEqual(m[3]?.size(), 5)
}

StdMapTestSuite.test("UnorderedMap.init(grouping:by:)") {
  let m = UnorderedMapGroup(
            grouping: [
                10, 11,
                20, 21, 22, 23,
                30, 31, 32, 33, 34, 35,
            ], by: { $0 / 10 })

  expectEqual(m.size(), 3)

  expectNil(m[0])
  expectEqual(m[1]?.size(), 2)
  expectEqual(m[2]?.size(), 4)
  expectEqual(m[3]?.size(), 6)
}

StdMapTestSuite.test("Map.subscript") {
  // This relies on the `std::map` conformance to `CxxDictionary` protocol.
  var m = initMap()
  let at1 = m[1]
  expectNotNil(at1)
  expectEqual(at1, 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
  expectNil(m[-1])
  expectNil(m[5])
  
  m[1] = 111
  expectEqual(m[1], 111)

  m[5] = 555
  expectEqual(m[5], 555)

  m[5] = nil
  expectNil(m[5])
  expectNil(m[5])
}

StdMapTestSuite.test("MapStrings.subscript") {
  var m = MapStrings()
  expectNil(m[std.string()])
  expectNil(m[std.string()])
  m[std.string()] = std.string()
  expectNotNil(m[std.string()])

  m[std.string("abc")] = std.string("qwe")
  expectEqual(m[std.string("abc")], std.string("qwe"))
}

StdMapTestSuite.test("NestedMap.subscript") {
  var m = NestedMap()
  expectNil(m[0])
  expectNil(m[0])
  m[1] = Map()
  expectNotNil(m[1])

  expectNil(m[1]![0])
  m[1]![0] = 123
  expectEqual(m[1]![0], 123)

  m[1]![0] = nil
  expectNil(m[1]![0])

  m[1] = nil
  expectNil(m[1])
}

StdMapTestSuite.test("UnorderedMap.subscript") {
  // This relies on the `std::unordered_map` conformance to `CxxDictionary` protocol.
  var m = initUnorderedMap()
  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
  expectNil(m[-1])
  expectNil(m[5])

  m[1] = 777
  expectEqual(m[1], 777)

  m[-1] = 228
  expectEqual(m[-1], 228)

  m[-1] = nil
  expectNil(m[-1])
  expectNil(m[-1])
}

StdMapTestSuite.test("Map.subscript(:default:)") {
  // This relies on the `std::map` conformance to `CxxDictionary` protocol.
  var m = initMap()
  let at1 = m[1, default: 0]
  expectNotNil(at1)
  expectEqual(at1, 3)
  expectEqual(m[2, default: 0], 2)
  expectEqual(m[3, default: 0], 3)
  expectEqual(m[-1, default: -1], -1)
  expectEqual(m[5, default: 5], 5)

  m[1] = nil
  expectEqual(m[-1, default: -1], -1)
  m[-1, default: -1] += 1
  expectEqual(m[-1], 0)
  m[-2, default: -2] += 3
  expectEqual(m[-2, default: -2], 1)

  m[-1] = nil
  expectEqual(m[-1, default: -1], -1)

  expectEqual(m[-5, default: 555], 555)
}

StdMapTestSuite.test("MapStrings.subscript(:default:)") {
  var m = MapStrings()
  expectNil(m[std.string()])
  expectNil(m[std.string()])
  m[std.string()] = std.string()
  expectNotNil(m[std.string()])

  m[std.string("abc")] = std.string("qw")
  expectEqual(m[std.string("abc")], std.string("qw"))

  m[std.string("abc"), default: std.string("qwe1")] = std.string("qwe")
  expectEqual(m[std.string("abc")], std.string("qwe"))
  m[std.string("abc"), default: std.string("qwe1")].append(std.string("2"))
  expectEqual(m[std.string("abc")], std.string("qwe2"))

  m[std.string("abcd"), default: std.string("qwe1")].append(std.string("1"))
  expectEqual(m[std.string("abcd")], std.string("qwe11"))

  expectEqual(m[std.string("abcdef"), default: std.string("abcdefg")], std.string("abcdefg"))
}

StdMapTestSuite.test("UnorderedMap.subscript(:default:)") {
  var m = initUnorderedMap()
  expectEqual(m[1, default: 0], 3)
  expectEqual(m[2, default: 0], 2)
  expectEqual(m[3, default: 0], 3)
  expectEqual(m[-1, default: -1], -1)
  expectEqual(m[5, default: 5], 5)

  m[1, default: 1] = 777
  expectEqual(m[1], 777)

  m[-1, default: -1] += 1
  expectEqual(m[-1], 0)
  m[-2, default: -2] += 3
  expectEqual(m[-2, default: -2], 1)

  expectEqual(m[-5, default: 555], 555)
}

StdMapTestSuite.test("Map.filter") {
  var m = initMap()
  var n = initEmptyMap()

  expectNotNil(m[1])
  expectEqual(n.size(), 0)

  m = m.filter { k, v in k != 1 }
  n = n.filter { k, v in false }
 
  expectNil(m[1])
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
  expectTrue(n.empty())
}

StdMapTestSuite.test("UnorderedMap.filter") {
  var m = initUnorderedMap()
  var n = initEmptyUnorderedMap()

  expectNotNil(m[1])
  expectEqual(n.size(), 0)

  m = m.filter { k, v in k != 1 }
  n = n.filter { k, v in false }

  expectNil(m[1])
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
  expectTrue(n.empty())
}

StdMapTestSuite.test("Map.erase") {
  var m = initMap()
  expectNotNil(m[1])
  m.erase(1)
  expectNil(m[1])
  m.erase(1)
  expectNil(m[1])
}

StdMapTestSuite.test("UnorderedMap.erase") {
  var m = initUnorderedMap()
  expectNotNil(m[2])
  m.erase(2)
  expectNil(m[2])
  m.erase(2)
  expectNil(m[2])
}

StdMapTestSuite.test("Map.remveValue(forKey:)") {
  var m = initMap()
  expectNotNil(m[1])
  expectEqual(m.removeValue(forKey: 1), 3)
  expectNil(m[1])
  expectEqual(m.removeValue(forKey: 1), nil)
  expectNil(m[1])
}

StdMapTestSuite.test("UnorderedMap.remveValue(forKey:)") {
  var m = initUnorderedMap()
  expectNotNil(m[2])
  expectEqual(m.removeValue(forKey: 2), 2)
  expectNil(m[2])
  expectEqual(m.removeValue(forKey: 2), nil)
  expectNil(m[2])
}

StdMapTestSuite.test("Map.merge<S: Sequence>(_ :)") {
  var m = initMap()
  m.merge([(1, 1), (2, 2), (3, 3)]) { v0, _ in v0 }

  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge([(1, 1), (2, 2), (3, 3)]) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("UnorderedMap.merge<S: Sequence>(_ :)") {
  var m = initUnorderedMap()
  m.merge([(1, 1), (2, 2), (3, 3)]) { v0, _ in v0 }
  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge([(1, 1), (2, 2), (3, 3)]) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("Map.merge") {
  var m = initMap()
  m.merge([1: 1, 2: 2, 3: 3]) { v0, _ in v0 }

  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge([1: 1, 2: 2, 3: 3]) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("UnorderedMap.merge") {
  var m = initUnorderedMap()
  m.merge([1: 1, 2: 2, 3: 3]) { v0, _ in v0 }
  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge([1: 1, 2: 2, 3: 3]) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("Map.merge(map)") {
  var m = initMap()

  let tmp = Map([1: 1, 2: 2, 3: 3])

  expectEqual(tmp[1], 1)
  expectEqual(tmp[2], 2)
  expectEqual(tmp[3], 3)

  m.merge(tmp) { v0, _ in v0 }
  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge(tmp) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("UnorderedMap.merge(map)") {
  var m = initUnorderedMap()
  let tmp = UnorderedMap([1: 1, 2: 2, 3: 3])
  expectEqual(tmp[1], 1)
  expectEqual(tmp[2], 2)
  expectEqual(tmp[3], 3)

  m.merge(tmp) { v0, _ in v0 }
  expectEqual(m[1], 3)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)

  m.merge(tmp) { _, v1 in v1 }
  expectEqual(m[1], 1)
  expectEqual(m[2], 2)
  expectEqual(m[3], 3)
}

StdMapTestSuite.test("Map.merge(CxxDictionary)") {
  // Empty map merging with empty map.
  var emptyMap = initEmptyMap()
  emptyMap.merge(initEmptyUnorderedMap()) { v0, _ in v0 }
  expectTrue(emptyMap.empty())

  emptyMap.merge(initEmptyUnorderedMap()) { _, v1 in v1 }
  expectTrue(emptyMap.empty())

  // Empty map merging with non-empty map.
  emptyMap.merge(initUnorderedMap()) { v0, _ in v0 }
  expectEqual(emptyMap[1], 3)
  expectEqual(emptyMap[2], 2)
  expectEqual(emptyMap[3], 3)

  emptyMap = initEmptyMap()
  emptyMap.merge(initUnorderedMap()) { _, v1 in v1 }
  expectEqual(emptyMap[1], 3)
  expectEqual(emptyMap[2], 2)
  expectEqual(emptyMap[3], 3)

  // Non-empty map merging with empty map.
  var map = initMap()
  map.merge(initUnorderedMap()) { v0, _ in v0 }
  expectEqual(emptyMap[1], 3)
  expectEqual(emptyMap[2], 2)
  expectEqual(emptyMap[3], 3)

  map.merge(initEmptyUnorderedMap()) { _, v1 in v1 }
  expectEqual(map[1], 3)
  expectEqual(map[2], 2)
  expectEqual(map[3], 3)

  // Non-empty map merging with non-empty map.
  let noneEmptydMap = UnorderedMap([1: 4, 2: 5, 3: 6, 4: 7])
  map.merge(noneEmptydMap) { v0, _ in v0 }
  expectEqual(map[1], 3)
  expectEqual(map[2], 2)
  expectEqual(map[3], 3)
  expectEqual(map[4], 7)

  map.merge(noneEmptydMap) { _, v1 in v1 }
  expectEqual(map[1], 4)
  expectEqual(map[2], 5)
  expectEqual(map[3], 6)
  expectEqual(map[4], 7)
}

StdMapTestSuite.test("UnorderedMap.merge(CxxDictionary)") {
  // Empty map merging with empty map.
  var emptyMap = initEmptyUnorderedMap()
  emptyMap.merge(initEmptyMap()) { v0, _ in v0 }
  expectTrue(emptyMap.empty())

  emptyMap.merge(initEmptyMap()) { _, v1 in v1 }
  expectTrue(emptyMap.empty())

  // Empty map merging with non-empty map.
  emptyMap.merge(initMap()) { v0, _ in v0 }
  expectEqual(emptyMap[1], 3)
  expectEqual(emptyMap[2], 2)
  expectEqual(emptyMap[3], 3)

  emptyMap = initEmptyUnorderedMap()
  emptyMap.merge(initMap()) { _, v1 in v1 }
  expectEqual(emptyMap[1], 3)
  expectEqual(emptyMap[2], 2)
  expectEqual(emptyMap[3], 3)

  // Non-empty map merging with empty map.
  var map = initUnorderedMap()
  map.merge(initEmptyMap()) { _, v1 in v1 }
  expectEqual(map[1], 3)
  expectEqual(map[2], 2)
  expectEqual(map[3], 3)

  map.merge(initEmptyMap()) { v0, _ in v0 }
  expectEqual(map[1], 3)
  expectEqual(map[2], 2)
  expectEqual(map[3], 3)

  // Non-empty map merging with non-empty map.
  let noneEmptyMap = Map([1: 4, 2: 5, 3: 6, 4: 7])

  map.merge(noneEmptyMap) { v0, _ in v0 }
  expectEqual(map[1], 3)
  expectEqual(map[2], 2)
  expectEqual(map[3], 3)
  expectEqual(map[4], 7)

  map.merge(noneEmptyMap) { _, v1 in v1 }
  expectEqual(map[1], 4)
  expectEqual(map[2], 5)
  expectEqual(map[3], 6)
  expectEqual(map[4], 7)
}

// `merging` is implemented by calling `merge`, so we can skip this test

runAllTests()

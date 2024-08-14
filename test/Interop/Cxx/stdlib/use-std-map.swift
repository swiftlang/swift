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
//
// REQUIRES: OS=macosx || OS=linux-gnu

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

runAllTests()

// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test

import StdlibUnittest
import StdHash
import CxxStdlib

var StdHashTestSuite = TestSuite("StdHash")

StdHashTestSuite.test("StdHash.stdBitset") {
  let dict: [Bitset : String] = [
    makeBitset(10) : "world",
    makeBitset(21) : "type",
    makeBitset(33) : "cxx"]

  expectEqual(dict[makeBitset(10)], "world")
  expectEqual(dict[makeBitset(21)], "type")
  expectEqual(dict[makeBitset(33)], "cxx")
}

// we need to manually instantiate std::hash<std::optional<std::string>>
StdHashTestSuite.test("StdHash.StdOptionalString") {
  let dict: [Optionalstr : String] = [
    Optionalstr.init("hello") : "world",
    Optionalstr.init("dependent"): "type",
    Optionalstr.init("swift"): "cxx",
    Optionalstr.init(): "nil"]

  expectEqual(dict[Optionalstr.init("hello")], "world")
  expectEqual(dict[Optionalstr.init("dependent")], "type")
  expectEqual(dict[Optionalstr.init("swift")], "cxx")
  expectEqual(dict[Optionalstr.init()], "nil")
}

StdHashTestSuite.test("StdHash.StructA") {
  var dict: [A : String] = [
    A.init(value: 10, comment: "hello") : "world",
    A.init(value: 20, comment: "dependent"): "type"
  ]

  dict[A.init(value: 10, comment: "mare")] = "nostrum"

  expectEqual(dict[A.init(value: 10, comment: "hello")], "nostrum")
}

StdHashTestSuite.test("StdHash.StructB") {
  var dict: [B : String] = [
    B.init(10, "hello") : "world",
    B.init(20, "dependent"): "type"
  ]

  dict[B.init(10, "mare")] = "nostrum"

  expectEqual(dict[B.init(10, "hello")], "nostrum")
}

runAllTests()

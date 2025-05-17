// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let equalTests = TestSuite("EqualTest")

equalTests.test("Equal Test 1") {
  let testCode = equal(1, 2, 3, 1, 5, default: -1)
  expectEqual([1], testCode)
}

equalTests.test("Equal Test 2") {
  let testCode = equal(1, 2, 3, 1, 5, 1, 5, 2, 4, 6, 32, 5, 2, default: -1)
  expectEqual([1, 2, 5], testCode)
}

equalTests.test("Equal Test 3") {
  let testCode = equal(5, 2, 9, default: -1)
  expectEqual([-1], testCode)
}

equalTests.test("Equal Test 4") {
  let testCode = equal("Goose", "is", "Goose", default: "Bye")
  expectEqual(["Goose"], testCode)
}

equalTests.test("Equal Test 5") {
  let testCode = equal("Jiaxu Li", "is", "Developer", default: "Goose")
  expectEqual(["Goose"], testCode)
}

equalTests.test("Equal Test 6") {
  let testCode = equal(1, 4, 5, 5, 2 ,5 ,2, 5, 25, 25, 6, 7, 10, 30, default: -10)
  expectEqual([5, 2, 25], testCode)
}

equalTests.test("Equal Test 7") {
  let testCode = equal(1.5, 13, 1.2, 1.5, 1.0, default: -1.0)
  expectEqual([1.5], testCode)
}

equalTests.test("Equal Test 8") {
  let testCode = equal("🪿", "💻", "🪿", "💻", "👓", default: "🇺🇸")
  expectEqual(["🪿", "💻"], testCode)
}

equalTests.test("Equal Test 9") {
  let testCode = equal(1, default: 10)
  expectEqual([10], testCode)
}

equalTests.test("Equal Test 10") {
  let testCode = equal(2, 32, 32, 5, 1, 7, 5, 2, 7, 5, 9, 3, 24, 6, 2, 24, 6, 5, default: -1)
  expectEqual([2, 32, 5, 7, 24, 6], testCode)
}

runAllTests()

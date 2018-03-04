// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Python runtime interop tests.

import Python
import StdlibUnittest

var PythonRuntimeTestSuite = TestSuite("PythonRuntime")

PythonRuntimeTestSuite.test("check-version") {
  let sysModule = try! Python.import("sys")
  let version = String(sysModule[member: "version"])!
  expectEqual("2.7.", version.prefix(4))
}

PythonRuntimeTestSuite.test("pylist") {
  let list = PyVal([0, 1, 2])
  expectEqual("[0, 1, 2]", list.description)
  expectEqual(3, Python.len.call(with: list))
  expectEqual("[0, 1, 2]", Python.str.call(with: list))
  expectEqual("<type 'list'>", Python.str.call(with: Python.type.call(with: list)))

  let polymorphicList = PyVal(["a", 2, true, 1.5])
  expectEqual("a", polymorphicList[0])
  expectEqual(2, polymorphicList[1])
  expectEqual(true, polymorphicList[2])
  expectEqual(1.5, polymorphicList[3])
  expectEqual(1.5, polymorphicList[-1])

  polymorphicList[2] = 2
  expectEqual(2, polymorphicList[2])
}

PythonRuntimeTestSuite.test("pydict") {
  let dict = ["a": 1, 1: 0.5] as PyVal
  expectEqual(2, Python.len.call(with: dict))
  expectEqual(1, dict["a"])
  expectEqual(0.5, dict[1])

  dict["b"] = "c"
  expectEqual("c", dict["b"])
  dict["b"] = "d"
  expectEqual("d", dict["b"])
}

PythonRuntimeTestSuite.test("binary-ops") {
  expectEqual(42, PyVal(42))
  expectEqual(42, PyVal(2) + PyVal(40))
  expectEqual(2, PyVal(2) * PyVal(3) + PyVal(-4))

  expectEqual("abcdef", PyVal("ab") + PyVal("cde") + PyVal("") + PyVal("f"))
  expectEqual("ababab", PyVal("ab") * 3)

  var x = PyVal(2)
  x += 3
  expectEqual(5, x)
  x *= 2
  expectEqual(10, x)
  x -= 3
  expectEqual(7, x)
  x /= 2
  expectEqual(3.5, x)
}

PythonRuntimeTestSuite.test("comparable") {
  let pyValArray: [PyVal] = [-1, 10, 1, 0, 0]
  expectEqual([-1, 0, 0, 1, 10], pyValArray.sorted())
  let pyValArray2: PyVal = [-1, 10, 1, 0, 0]
  expectEqual([-1, 0, 0, 1, 10], pyValArray2.sorted())
  let pyValArray3: [PyVal] = ["a", 10, "b", "b", 0]
  expectEqual([0, 10, "a", "b", "b"], pyValArray3.sorted())
}

PythonRuntimeTestSuite.test("range-iter") {
  for (index, val) in Python.range.call(with: 5).enumerated() {
    expectEqual(PyVal(index), val)
  }
}

runAllTests()

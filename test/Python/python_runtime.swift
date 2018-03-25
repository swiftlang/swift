// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Python runtime interop tests.

import Python
import StdlibUnittest

var PythonRuntimeTestSuite = TestSuite("PythonRuntime")

PythonRuntimeTestSuite.test("check-version") {
  let sysModule = Python.import("sys")
  let version = String(sysModule.version)!
  expectEqual("2.7.", version.prefix(4))
}

PythonRuntimeTestSuite.test("pylist") {
  let list: PyValue = [0, 1, 2]
  expectEqual("[0, 1, 2]", list.description)
  expectEqual(3, Python.len.call(with: list))
  expectEqual("[0, 1, 2]", Python.str.call(with: list))
  expectEqual("<type 'list'>", Python.str.call(with: Python.type.call(with: list)))

  let polymorphicList = PyValue(["a", 2, true, 1.5])
  expectEqual("a", polymorphicList[0])
  expectEqual(2, polymorphicList[1])
  expectEqual(true, polymorphicList[2])
  expectEqual(1.5, polymorphicList[3])
  expectEqual(1.5, polymorphicList[-1])

  polymorphicList[2] = 2
  expectEqual(2, polymorphicList[2])
}

PythonRuntimeTestSuite.test("pydict") {
  let dict: PyValue = ["a": 1, 1: 0.5]
  expectEqual(2, Python.len.call(with: dict))
  expectEqual(1, dict["a"])
  expectEqual(0.5, dict[1])

  dict["b"] = "c"
  expectEqual("c", dict["b"])
  dict["b"] = "d"
  expectEqual("d", dict["b"])
}

PythonRuntimeTestSuite.test("binary-ops") {
  expectEqual(42, PyValue(42))
  expectEqual(42, PyValue(2) + PyValue(40))
  expectEqual(2, PyValue(2) * PyValue(3) + PyValue(-4))

  expectEqual("abcdef", PyValue("ab") + PyValue("cde") + PyValue("") + PyValue("f"))
  expectEqual("ababab", PyValue("ab") * 3)

  var x = PyValue(2)
  x += 3
  expectEqual(5, x)
  x *= 2
  expectEqual(10, x)
  x -= 3
  expectEqual(7, x)
  x /= 2
  expectEqual(3.5, x)
  x += -1
  expectEqual(2.5, x)
}

PythonRuntimeTestSuite.test("comparable") {
  let pyValArray: [PyValue] = [-1, 10, 1, 0, 0]
  expectEqual([-1, 0, 0, 1, 10], pyValArray.sorted())
  let pyValArray2: PyValue = [-1, 10, 1, 0, 0]
  expectEqual([-1, 0, 0, 1, 10], pyValArray2.sorted())
  let pyValArray3: [PyValue] = ["a", 10, "b", "b", 0]
  expectEqual([0, 10, "a", "b", "b"], pyValArray3.sorted())
}

PythonRuntimeTestSuite.test("range-iter") {
  for (index, val) in Python.range.call(with: 5).enumerated() {
    expectEqual(PyValue(index), val)
  }
}

PythonRuntimeTestSuite.test("errors") {
  expectThrows(PythonError.exception("division by zero") as PythonError?, {
    try PyValue(1).throwing.callMember("__truediv__", with: 0)
    // expectThrows does not fail if no error is thrown.
    fatalError("No error was thrown.")
  })
  expectThrows(PythonError.invalidMember("undefinedMember") as PythonError?, {
    try PyValue(1).throwing.callMember("undefinedMember", with: 0)
    fatalError("No error was thrown.")
  })
}

PythonRuntimeTestSuite.test("tuple") {
  let element1: PyValue = 0
  let element2: PyValue = "abc"
  let element3: PyValue = [0, 0]
  let element4: PyValue = ["a": 0, "b": "c"]
  let pair = PyValue(tuple: element1, element2)
  let (pair1, pair2) = pair.tuple2
  expectEqual(element1, pair1)
  expectEqual(element2, pair2)

  let triple = PyValue(tuple: element1, element2, element3)
  let (triple1, triple2, triple3) = triple.tuple3
  expectEqual(element1, triple1)
  expectEqual(element2, triple2)
  expectEqual(element3, triple3)

  let quadruple = PyValue(tuple: element1, element2, element3, element4)
  let (quadruple1, quadruple2, quadruple3, quadruple4) = quadruple.tuple4
  expectEqual(element1, quadruple1)
  expectEqual(element2, quadruple2)
  expectEqual(element3, quadruple3)
  expectEqual(element4, quadruple4)

  expectEqual(element2, quadruple[1])
}

PythonRuntimeTestSuite.test("python-convertible") {
  // Ensure that we cover the -1 case as this is used by Python
  // to signal conversion errors.
  let minusOne: PyValue = -1
  let zero: PyValue = 0
  let half: PyValue = 0.5
  let string: PyValue = "abc"

  expectEqual(-1, Int(minusOne))
  expectEqual(0, Int(zero))
  expectEqual("abc", String(string))
  expectEqual(-1.0, Double(minusOne))
  expectEqual(0.0, Double(zero))
  expectEqual(0.5, Double(half))
  // Python rounds down the value in this case.
  expectEqual(0, Int(half))

  expectNil(String(zero))
  expectNil(Int(string))
  expectNil(Double(string))
}

runAllTests()

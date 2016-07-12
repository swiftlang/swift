// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

struct StructWithMembers {
  var a = 1
  var b = "Hello World"
}

class ClassWithMembers {
  var a = 1
  var b = "Hello World"
}

class ClassWithMirror: CustomReflectable {
  var customMirror: Mirror {
    return Mirror(self, children: ["a" : 1, "b" : "Hello World"])
  }
}

let PrintForDebuggerTests = TestSuite("PrintForDebugger")
PrintForDebuggerTests.test("StructWithMembers") {
  let printed = _PrintForDebugger.printForDebugger(value: StructWithMembers())
  expectEqual(printed, "▿ StructWithMembers\n  - a : 1\n  - b : \"Hello World\"\n")
}

PrintForDebuggerTests.test("ClassWithMembers") {
  let printed = _PrintForDebugger.printForDebugger(value: ClassWithMembers())
  expectTrue(printed.hasPrefix("<ClassWithMembers: 0x"))
}

PrintForDebuggerTests.test("ClassWithMirror") {
  let printed = _PrintForDebugger.printForDebugger(value: ClassWithMirror())
  expectEqual(printed, "▿ ClassWithMirror\n  - a : 1\n  - b : \"Hello World\"\n")
}

PrintForDebuggerTests.test("Array") {
  let printed = _PrintForDebugger.printForDebugger(value: [1,2,3,4])
  expectEqual(printed, "▿ 4 elements\n  - 0 : 1\n  - 1 : 2\n  - 2 : 3\n  - 3 : 4\n")
}

PrintForDebuggerTests.test("Dictionary") {
  let printed = _PrintForDebugger.printForDebugger(value: [1:2])
  expectEqual(printed, "▿ 1 elements\n  ▿ 0 : 2 elements\n    - .0 : 1\n    - .1 : 2\n")
}

PrintForDebuggerTests.test("NilOptional") {
  let printed = _PrintForDebugger.printForDebugger(value: nil as Int?)
  expectTrue(printed.hasPrefix("nil"))
}

PrintForDebuggerTests.test("SomeOptional") {
  let printed = _PrintForDebugger.printForDebugger(value: 3 as Int?)
  expectEqual(printed, "▿ Optional<Int>\n  - some : 3\n")
}

runAllTests()

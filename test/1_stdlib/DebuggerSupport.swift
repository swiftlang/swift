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

#if _runtime(_ObjC)
struct DontBridgeThisStruct {
  var message = "Hello World"
}

extension DontBridgeThisStruct : _ObjectiveCBridgeable {
  typealias _ObjectiveCType = AnyObject

  func _bridgeToObjectiveC() -> _ObjectiveCType {
    fatalError("tried to bridge DontBridgeThisStruct")
  }

  static func _forceBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout DontBridgeThisStruct?
  ) {
    result = nil
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ source: _ObjectiveCType,
    result: inout DontBridgeThisStruct?
  ) -> Bool {
    result = nil
    return false
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: _ObjectiveCType?)
      -> DontBridgeThisStruct {
    return DontBridgeThisStruct()
  }
}
#endif

let StringForPrintObjectTests = TestSuite("StringForPrintObject")
StringForPrintObjectTests.test("StructWithMembers") {
  let printed = _DebuggerSupport.stringForPrintObject(StructWithMembers())
  expectEqual(printed, "▿ StructWithMembers\n  - a : 1\n  - b : \"Hello World\"\n")
}

#if _runtime(_ObjC)
StringForPrintObjectTests.test("ClassWithMembers") {
  let printed = _DebuggerSupport.stringForPrintObject(ClassWithMembers())
  expectTrue(printed.hasPrefix("<ClassWithMembers: 0x"))
}
#endif

StringForPrintObjectTests.test("ClassWithMirror") {
  let printed = _DebuggerSupport.stringForPrintObject(ClassWithMirror())
  expectEqual(printed, "▿ ClassWithMirror\n  - a : 1\n  - b : \"Hello World\"\n")
}

StringForPrintObjectTests.test("Array") {
  let printed = _DebuggerSupport.stringForPrintObject([1,2,3,4])
  expectEqual(printed, "▿ 4 elements\n  - 0 : 1\n  - 1 : 2\n  - 2 : 3\n  - 3 : 4\n")
}

StringForPrintObjectTests.test("Dictionary") {
  let printed = _DebuggerSupport.stringForPrintObject([1:2])
  expectEqual(printed, "▿ 1 element\n  ▿ 0 : 2 elements\n    - .0 : 1\n    - .1 : 2\n")
}

StringForPrintObjectTests.test("NilOptional") {
  let printed = _DebuggerSupport.stringForPrintObject(nil as Int?)
  expectEqual(printed, "nil\n")
}

StringForPrintObjectTests.test("SomeOptional") {
  let printed = _DebuggerSupport.stringForPrintObject(3 as Int?)
  expectEqual(printed, "▿ Optional<Int>\n  - some : 3\n")
}

#if _runtime(_ObjC)
StringForPrintObjectTests.test("DontBridgeThisStruct") {
  let printed = _DebuggerSupport.stringForPrintObject(DontBridgeThisStruct())
  expectEqual(printed, "▿ DontBridgeThisStruct\n  - message : \"Hello World\"\n")
}
#endif

runAllTests()

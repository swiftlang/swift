// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s

// Fake ObjectiveC module for testing String/NSString bridging.

struct ObjCBool : LogicValue {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  func [conversion] __conversion() -> Bool {
    return this
  }
}

func [asmname="swift_BoolToObjCBool"]
convertBoolToObjCBool(x:Bool) -> ObjCBool

func [asmname="swift_ObjCBoolToBool"]
convertObjCBoolToBool(x:ObjCBool) -> Bool

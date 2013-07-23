// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -triple x86_64-apple-darwin10 -emit-llvm -g %s | FileCheck %s
// REQUIRES: sdk
// CHECK: DW_TAG_compile_unit

// Fake ObjectiveC module for testing String/NSString bridging.

import ObjectiveC

struct ObjCSel { var ptr : COpaquePointer }

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


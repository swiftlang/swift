// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep ObjC.pcm

import ObjC // Clang module

struct ObjCBool {
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

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  func [conversion] __conversion() -> ObjCBool {
    var result : ObjCBool
    if this { result.value = 1 }
    else { result.value = 0 }
    return result
  }
}

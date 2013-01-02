import objc
import Foundation
import ScriptingBridge // Clang module

//===----------------------------------------------------------------------===//
// Typed Collections
//===----------------------------------------------------------------------===//
struct SBTypedElementArray<T : NSObject> {
  var array : SBElementArray

  constructor(array : SBElementArray) { this.array = array }

  func [conversion] __conversion() -> SBElementArray { return array }

  subscript (idx : NSUInteger) -> T {
    get {
      return T(array[idx])
    }
  }

  func objectWithName(name : NSString) -> T {
    return T(array.objectWithName(name))
  }
}

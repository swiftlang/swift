// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep objc.pcm

import objc
import Foundation

struct NSTypedArray<T : NSObject> {
  var array : NSArray

  constructor(array : NSArray) { this.array = array }

  func [conversion] __conversion() -> NSArray { return array }

  subscript (idx : CUnsignedInt) -> T {
    get {
      return T(array[idx])
    }
  }
}

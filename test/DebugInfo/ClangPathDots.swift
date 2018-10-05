// REQUIRES: objc_interop
// RUN: rm -rf %t.cache
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs -o - \
// RUN:   -module-cache-path %t.cache | %FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs -o - \
// RUN:   -module-cache-path %t.cache | %FileCheck %s --check-prefix=CACHED

// FIRST: !DIFile(filename: "NSObject.h", directory: {{.*}}/include/objc")
// CACHED: !DIFile(filename: "NSObject.h", directory: {{.*}}/include/objc")

import ObjectiveC

extension NSObject : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public var _cVarArgEncoding: [Int] {
    _autorelease(self)
    return _encodeBitsAsWords(self)
  }
}


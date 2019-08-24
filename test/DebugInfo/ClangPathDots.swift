// REQUIRES: objc_interop
// RUN: rm -rf %t.cache
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs -o - \
// RUN:   -module-cache-path %t.cache | %FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs -o - \
// RUN:   -module-cache-path %t.cache | %FileCheck %s --check-prefix=CACHED

// Test that the paths have no extra "./" components on rebuild.

// FIRST: !DIFile(filename: "{{.*}}/include/objc/NSObject.h",
// CACHED: !DIFile(filename: "{{.*}}/include/objc/NSObject.h",

import ObjectiveC

extension NSObject : CVarArg {
  /// Transform `self` into a series of machine words that can be
  /// appropriately interpreted by C varargs
  public var _cVarArgEncoding: [Int] {
    _autorelease(self)
    return _encodeBitsAsWords(self)
  }
}


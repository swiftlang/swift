// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s

// REQUIRES: objc_interop

import Foundation

// rdar://15933538 let decls with StoredObjC storage type should have a getter
// synthesized, but not a setter.
@objc
class C : NSObject {
  let x : Int = 100
}

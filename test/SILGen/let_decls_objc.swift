// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen
//
// REQUIRES: objc_interop

import Foundation

// rdar://15933538 let decls with StoredObjC storage type should have a getter
// synthesized, but not a setter.
@objc
class C : NSObject {
  let x : Int = 100
}

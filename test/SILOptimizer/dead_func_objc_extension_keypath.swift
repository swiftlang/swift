// RUN: %target-swift-frontend %s -O -emit-sil -import-objc-header %S/Inputs/keypaths_objc.h
// REQUIRES: objc_interop

import Foundation
public func test_nocrash_rdar34913689() {
  _ = \ObjCFoo.objcExtraProp
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  func foo(_ i: Int) -> Double
  func foo(_ d: Double) -> Double

  @objc optional func opt(_ i: Int) -> Int
  @objc optional func opt(_ d: Double) -> Int
}

func testOptional(obj: P) {
  // CHECK: common result type for {{.*}} is Int
  _ = obj.opt?(1)

  // CHECK: common result type for {{.*}} is Int
  _ = obj.opt!(1)
}

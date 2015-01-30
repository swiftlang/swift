// RUN: %target-swift-ide-test -annotate -source-filename %s | FileCheck %s

// REQUIRES: objc_interop

// CHECK: import <iMod>Foundation</iMod>
import Foundation

// CHECK: import <iMod>ObjectiveC</iMod>.<iMod>NSObject</iMod>
import ObjectiveC.NSObject

func foo(c1: NSObject, c2: NSObject) {
  // CHECK: <Param@[[@LINE-1]]:10>c1</Param> <iFunc@>==</iFunc> <Param@[[@LINE-1]]:24>c2</Param>
  c1 == c2
}

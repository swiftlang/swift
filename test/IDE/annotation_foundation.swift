// RUN: rm -rf %t.mcp
// RUN: %swift-ide-test -annotate -source-filename %s -sdk %sdk -module-cache-path %t.mcp | FileCheck %s

// CHECK: import <iMod>Foundation</iMod>
import Foundation

// CHECK: import <iMod>ObjectiveC</iMod>.<iMod>NSObject</iMod>
import ObjectiveC.NSObject

func foo(c1: NSObject, c2: NSObject) {
  // CHECK: <Param@[[@LINE-1]]:10>c1</Param> <iFunc@>==</iFunc> <Param@[[@LINE-1]]:24>c2</Param>
  c1 == c2
}

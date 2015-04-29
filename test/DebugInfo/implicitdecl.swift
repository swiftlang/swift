// RUN: %target-swift-frontend -emit-ir -g %s -o - | FileCheck %s

// REQUIRES: objc_interop

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "Protocol",
// CHECK-SAME:             scope: ![[ObjectiveC:[0-9]+]]
// CHECK-SAME:             identifier: "_TtCSo8Protocol"
// CHECK: ![[ObjectiveC]] = !MDModule(name: "ObjectiveC"
import Foundation

public func f() {
  var protocolObject = NSProtocolFromString("HelperTool")!
}


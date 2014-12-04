// RUN: %target-build-swift -emit-ir -g %s -o - | FileCheck %s
// CHECK: \0030"{{, [^,]+}}, metadata ![[ObjectiveC:[0-9]+]], {{.*}}metadata !"_TtCSo8Protocol"} ; [ DW_TAG_structure_type ] [Protocol]
// CHECK: ![[ObjectiveC]] = metadata !{{.*}} ; [ DW_TAG_module ] [ObjectiveC] [line 1]
import Foundation

func f() {
  var protocolObject = NSProtocolFromString("HelperTool")!
}

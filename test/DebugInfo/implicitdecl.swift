// RUN: %target-build-swift -module-cache-path %t/clang-module-cache -emit-ir -g %s -o - | FileCheck %s
// CHECK: metadata ![[ObjectiveC:[0-9]+]], metadata !"_TtCSo8Protocol", {{.*}} ; [ DW_TAG_structure_type ] [_TtCSo8Protocol]
// CHECK: ![[ObjectiveC]] = metadata !{{.*}} ; [ DW_TAG_namespace ] [ObjectiveC]
import Cocoa

func f() {
	var protocolObject = NSProtocolFromString("HelperTool")
}

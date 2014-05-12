// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-build-swift -module-cache-path %t/clang-module-cache -emit-ir -g %s -o - | FileCheck %s
// CHECK: i32 {{.*}}, metadata !{{[0-9]+}}, metadata ![[ObjectiveC:[0-9]+]], {{.*}}metadata !"_TtCSo8Protocol"} ; [ DW_TAG_structure_type ] [Protocol]
// CHECK: ![[ObjectiveC]] = metadata !{{.*}} ; [ DW_TAG_module ] [ObjectiveC]
import Foundation

func f() {
	var protocolObject = NSProtocolFromString("HelperTool")!
}

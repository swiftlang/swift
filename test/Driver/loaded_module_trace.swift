// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -module-name Module %S/Inputs/loaded_module_trace_empty.swift -o %t/Module.swiftmodule
// RUN: %target-build-swift -emit-module -module-name Module2 %S/Inputs/loaded_module_trace_imports_module.swift -o %t/Module2.swiftmodule -I %t
// RUN: %target-build-swift %s -emit-loaded-module-trace -o %t/loaded_module_trace -I %t
// RUN: %FileCheck -check-prefix=CHECK %s < %t/loaded_module_trace.trace.json
// RUN: %FileCheck -check-prefix=CHECK-CONFIRM-ONELINE %s < %t/loaded_module_trace.trace.json

// CHECK: {
// CHECK: "name":"loaded_module_trace"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module2.swiftmodule"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK: ]
// CHECK: }

// Make sure it's all on a single line.
// CHECK-CONFIRM-ONELINE: {"name":{{.*}}]}

// 'Module2' imports 'Module', so we're also checking we don't get transitive dependencies.
import Module2

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -emit-module -module-name Module %S/Inputs/loaded_module_trace_empty.swift -o %t/Module.swiftmodule
// RUN: %target-build-swift %s -emit-loaded-module-trace -o %t/loaded_module_trace -I %t
// RUN: %FileCheck %s < %t/loaded_module_trace.trace.json

// CHECK: {
// CHECK: "name":"loaded_module_trace"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK: "{{[^"]*}}/Module.swiftmodule"
// CHECK: "{{[^"]*}}/Swift.swiftmodule"
// CHECK: "{{[^"]*}}/SwiftOnoneSupport.swiftmodule"
// CHECK: ]
// CHECK: }

import Module

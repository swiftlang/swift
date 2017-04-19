// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -emit-module -module-name Module %S/Inputs/loaded_module_trace_empty.swift -o %t/Module.swiftmodule
// RUN: %target-swift-frontend -c %s -emit-loaded-module-trace -emit-loaded-module-trace-path - -I %t | %FileCheck %s

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

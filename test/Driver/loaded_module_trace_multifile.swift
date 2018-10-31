// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -module-name Module %S/Inputs/loaded_module_trace_empty.swift -o %t/Module.swiftmodule
// RUN: %target-build-swift -emit-module -module-name Module2 %S/Inputs/loaded_module_trace_empty.swift -o %t/Module2.swiftmodule
// RUN: %target-build-swift %s %S/Inputs/loaded_module_trace_imports_module.swift -emit-loaded-module-trace-path %t/multifile.trace.json -emit-library -o %t/loaded_module_trace_multifile -I %t
// RUN: %FileCheck %s < %t/multifile.trace.json

// This file only imports Module2, but the other file imports Module: hopefully they both appear!

// CHECK: {
// CHECK: "name":"loaded_module_trace_multifile"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK: "{{[^"]*[/\\]}}Module2.swiftmodule"
// CHECK: "{{[^"]*[/\\]}}Module.swiftmodule"
// CHECK: "{{[^"]*[/\\]}}Swift.swiftmodule"
// CHECK: "{{[^"]*[/\\]}}SwiftOnoneSupport.swiftmodule"
// CHECK: ]
// CHECK: }

import Module2

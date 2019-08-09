// RUN: %empty-directory(%t)
// RUN: %target-build-swift -enable-library-evolution -swift-version 5 -emit-module-interface-path %t/Module.swiftinterface -module-name Module %S/Inputs/loaded_module_trace_empty.swift
// RUN: %target-build-swift -swift-version 5 -emit-module -module-name Module2 %S/Inputs/loaded_module_trace_imports_module.swift -o %t/Module2.swiftmodule -I %t
// RUN: %target-build-swift %s -emit-loaded-module-trace -o %t/loaded_module_trace_swiftinterface -I %t
// RUN: %FileCheck -check-prefix=CHECK %s < %t/loaded_module_trace_swiftinterface.trace.json

// CHECK: {
// CHECK: "version":2
// CHECK: "name":"loaded_module_trace_swiftinterface"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module2.swiftmodule"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module.swiftinterface"
// CHECK: ]
// CHECK: "swiftmodulesDetailedInfo":[
// CHECK-DAG: {"name":"Module2","path":"{{[^"]*\\[/\\]}}Module2.swiftmodule","isImportedDirectly":true,"supportsLibraryEvolution":false}
// CHECK-DAG: {"name":"Swift","path":"{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true}
// CHECK-DAG: {"name":"SwiftOnoneSupport","path":"{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true}
// CHECK-DAG: {"name":"Module","path":"{{[^"]*\\[/\\]}}Module.swiftinterface","isImportedDirectly":false,"supportsLibraryEvolution":true}
// CHECK: ]
// CHECK: }

import Module2

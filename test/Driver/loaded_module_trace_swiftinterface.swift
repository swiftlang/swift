// REQUIRES: rdar64941662
// UNSUPPORTED: -windows-msvc

// 1) If there is no swiftmodule, use the swiftinterface
//
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -enable-library-evolution -module-name Module %S/Inputs/loaded_module_trace_empty.swift -emit-module-interface-path %t/Module.swiftinterface
// RUN: %target-build-swift -swift-version 5 -I %t -module-name Module2 %S/Inputs/loaded_module_trace_imports_module.swift -emit-module -o %t/Module2.swiftmodule
// RUN: %target-build-swift -I %t %s -emit-loaded-module-trace -o %t/loaded_module_trace_swiftinterface
// RUN: %FileCheck -check-prefix=CHECK %s < %t/loaded_module_trace_swiftinterface.trace.json
//
// CHECK: {
// CHECK: "version":2
// CHECK: "name":"loaded_module_trace_swiftinterface"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module2.swiftmodule"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module.swiftinterface"
// CHECK-NOT: Module.swiftmodule
// CHECK: ]
// CHECK: "swiftmodulesDetailedInfo":[
// CHECK-DAG: {"name":"Module2","path":"{{[^"]*\\[/\\]}}Module2.swiftmodule","isImportedDirectly":true,"supportsLibraryEvolution":false}
// CHECK-DAG: {"name":"Swift","path":"{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true}
// CHECK-DAG: {"name":"SwiftOnoneSupport","path":"{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true}
// CHECK-DAG: {"name":"Module","path":"{{[^"]*\\[/\\]}}Module.swiftinterface","isImportedDirectly":false,"supportsLibraryEvolution":true}
// CHECK: ]
// CHECK: }


// 2) If there is a swiftmodule next to the swiftinterface, use that instead.
//
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -enable-library-evolution -module-name Module %S/Inputs/loaded_module_trace_empty.swift -emit-module-interface-path %t/Module.swiftinterface -emit-module -o %t/Module.swiftmodule
// RUN: %target-swift-frontend -swift-version 5 -I %t -module-name Module2 %S/Inputs/loaded_module_trace_imports_module.swift -emit-module -o %t/Module2.swiftmodule
// RUN: %target-build-swift -I %t %s -emit-loaded-module-trace -o %t/loaded_module_trace_swiftinterface
// RUN: %FileCheck -check-prefix=ADJMODULE %s < %t/loaded_module_trace_swiftinterface.trace.json
//
// ADJMODULE: Module.swiftmodule
// ADJMODULE-NOT: Module.swiftinterface


// 3) If there is a swiftmodule in the (non-prebuilt) module cache path pointing to the swiftinterface, use the swiftinterface.
//
// RUN: %empty-directory(%t)
// RUN: mkdir %t/MCP
// RUN: %target-build-swift -swift-version 5 -enable-library-evolution -module-cache-path %t/MCP -module-name Module %S/Inputs/loaded_module_trace_empty.swift -emit-module-interface-path %t/Module.swiftinterface -emit-module -o %t/MCP/Module.swiftmodule
// RUN: %target-swift-frontend -swift-version 5 -I %t -module-cache-path %t/MCP -module-name Module2  %S/Inputs/loaded_module_trace_imports_module.swift -emit-module -o %t/Module2.swiftmodule
// RUN: %target-build-swift -I %t -module-cache-path %t/MCP %s -emit-loaded-module-trace -o %t/loaded_module_trace_swiftinterface
// RUN: %FileCheck -check-prefix=CACHEDMODULE %s < %t/loaded_module_trace_swiftinterface.trace.json
//
// CACHEDMODULE: Module.swiftinterface
// CACHEDMODULE-NOT: Module.swiftmodule


// 4) If there is a swiftmodule in the prebuilt module cache path pointing to the swiftinterface, use the swiftinterface.
//
// RUN: %empty-directory(%t)
// RUN: mkdir %t/MCP %t/PB %t/Main
// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -prebuilt-module-cache-path %t/PB -module-cache-path %/MCP -module-name Module %S/Inputs/loaded_module_trace_empty.swift -emit-module -o %t/PB/Module.swiftmodule -emit-module-interface-path %t/Main/Module.swiftinterface
// RUN: %target-swift-frontend -swift-version 5 -I %t/Main -prebuilt-module-cache-path %t/PB -module-cache-path %t/MCP -module-name Module2 %S/Inputs/loaded_module_trace_imports_module.swift -emit-module -o %t/Main/Module2.swiftmodule
// RUN: %target-build-swift -I %t/Main -module-cache-path %t/MCP %s -emit-loaded-module-trace -o %t/loaded_module_trace_swiftinterface
// RUN: %FileCheck -check-prefix=PREBUILTMODULE %s < %t/loaded_module_trace_swiftinterface.trace.json
//
// PREBUILTMODULE: Module.swiftinterface
// PREBUILTMODULE-NOT: Module.swiftmodule

import Module2

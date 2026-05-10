// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift -emit-module -module-name Module %S/Inputs/loaded_module_trace_empty.swift -o %t/Module.swiftmodule -module-cache-path %t/cache
// RUN: %target-build-swift -emit-module -module-name Module2 %S/Inputs/loaded_module_trace_empty.swift -o %t/Module2.swiftmodule -module-cache-path %t/cache
// RUN: %target-build-swift %s %S/Inputs/loaded_module_trace_imports_module.swift -emit-loaded-module-trace-path %t/multifile.trace.json -emit-library -o %t/loaded_module_trace_multifile -I %t -module-cache-path %t/cache
// RUN: %FileCheck %s < %t/multifile.trace.json

// This file only imports Module2, but the other file imports Module: hopefully they both appear!
// The difference between this test and the one in loaded_module_trace is that here, we test that
// dependencies from multiple files in the same module are accounted for correctly. As a result,
// `Module.swiftmodule` is marked as directly imported here.

// CHECK: {
// CHECK: "version":2
// CHECK: "name":"loaded_module_trace_multifile"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "languageMode":"4"
// CHECK: "enabledLanguageFeatures":[
// CHECK: ]
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module2.swiftmodule"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*\\[/\\]}}Module.swiftmodule"
// CHECK: ]
// CHECK: "swiftmodulesDetailedInfo":[
// CHECK-DAG: {"name":"Module2","path":"{{[^"]*\\[/\\]}}Module2.swiftmodule","isImportedDirectly":true,"supportsLibraryEvolution":false,"strictMemorySafety":false}
// CHECK-DAG: {"name":"Swift","path":"{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true,"strictMemorySafety":true}
// CHECK-DAG: {"name":"SwiftOnoneSupport","path":"{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}","isImportedDirectly":true,"supportsLibraryEvolution":true,"strictMemorySafety":true}
// CHECK-DAG: {"name":"Module","path":"{{[^"]*\\[/\\]}}Module.swiftmodule","isImportedDirectly":true,"supportsLibraryEvolution":false,"strictMemorySafety":false}
// CHECK: ]
// CHECK: }

import Module2

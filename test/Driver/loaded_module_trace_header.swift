// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t/trace %target-build-swift -module-name loaded_module_trace_header -c %s -o- -import-objc-header %S/Inputs/loaded_module_trace_header.h -module-cache-path %t/cache > /dev/null
// RUN: %FileCheck %s < %t/trace

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_header"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*}}/ObjectiveC.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Dispatch.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Darwin.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Swift.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*}}/SwiftOnoneSupport.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: ]
// CHECK: "swiftmodulesDetailedInfo":[
// CHECK: {
// CHECK-DAG: "name":"Swift"
// CHECK-DAG: "path":"{{[^"]*\\[/\\]}}Swift.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "isImportedDirectly":true
// CHECK-DAG: "supportsLibraryEvolution":true
// CHECK: }
// CHECK: {
// CHECK-DAG: "name":"SwiftOnoneSupport"
// CHECK-DAG: "path":"{{[^"]*\\[/\\]}}SwiftOnoneSupport.swiftmodule{{(\\[/\\][^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "isImportedDirectly":true
// CHECK-DAG: "supportsLibraryEvolution":true
// CHECK: }
// CHECK: ]

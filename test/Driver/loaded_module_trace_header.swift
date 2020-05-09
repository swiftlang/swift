// RUN: rm -f %t
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_header -c %s -o- -import-objc-header %S/Inputs/loaded_module_trace_header.h > /dev/null
// RUN: %FileCheck %s < %t

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_header"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK-DAG: "{{[^"]*}}/ObjectiveC.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*}}/Dispatch.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*}}/Darwin.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
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

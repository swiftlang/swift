// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: %target-build-swift -o %t/trace -module-name loaded_module_trace_foundation %s -emit-loaded-module-trace -emit-loaded-module-trace-path - -module-cache-path %t/cache 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_foundation"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[

// Swift and SwiftOnoneSupport is expected to be locally built;
// everything else comes from the SDK, built from swiftinterface.

// CHECK-DAG: "{{[^"]*}}/ObjectiveC.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Dispatch.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Darwin.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "{{[^"]*}}/Swift.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK-DAG: "{{[^"]*}}/SwiftOnoneSupport.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: ]
// CHECK: "swiftmodulesDetailedInfo":[
// CHECK: {
// CHECK-DAG: "name":"Foundation"
// CHECK-DAG: "path":"{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swift(module|interface))?}}"
// CHECK-DAG: "isImportedDirectly":true
// CHECK-DAG: "supportsLibraryEvolution":true
// CHECK: }
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

import Foundation

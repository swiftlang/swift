// RUN: %target-build-swift -o %t -module-name loaded_module_trace_foundation %s -emit-loaded-module-trace -emit-loaded-module-trace-path - 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_foundation"
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
// CHECK-DAG: "name":"Foundation"
// CHECK-DAG: "path":"{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
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

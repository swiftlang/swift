// RUN: %target-build-swift -o %t -module-name loaded_module_trace_foundation %s -emit-loaded-module-trace -emit-loaded-module-trace-path - 2>&1 | %FileCheck %s

// Disabled for now
// REQUIRES: rdar://problem/31771633

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_foundation"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK: "{{[^"]*}}/ObjectiveC.swiftmodule"
// CHECK: "{{[^"]*}}/Dispatch.swiftmodule"
// CHECK: "{{[^"]*}}/Darwin.swiftmodule"
// CHECK: "{{[^"]*}}/Foundation.swiftmodule"
// CHECK: "{{[^"]*}}/Swift.swiftmodule"
// CHECK: "{{[^"]*}}/IOKit.swiftmodule"
// CHECK: "{{[^"]*}}/SwiftOnoneSupport.swiftmodule"
// CHECK: ]
// CHECK: }

import Foundation

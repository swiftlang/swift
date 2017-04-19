// RUN: %target-swift-frontend -c %s -emit-loaded-module-trace -emit-loaded-module-trace-path - | %FileCheck %s

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

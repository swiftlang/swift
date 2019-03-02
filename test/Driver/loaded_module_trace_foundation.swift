// RUN: %target-build-swift -o %t -module-name loaded_module_trace_foundation %s -emit-loaded-module-trace -emit-loaded-module-trace-path - 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_foundation"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK: "{{[^"]*}}/ObjectiveC.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: "{{[^"]*}}/Dispatch.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: "{{[^"]*}}/Darwin.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: "{{[^"]*}}/Foundation.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: "{{[^"]*}}/Swift.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: "{{[^"]*}}/SwiftOnoneSupport.swiftmodule{{(\\/[^"]+[.]swiftmodule)?}}"
// CHECK: ]
// CHECK: }

import Foundation

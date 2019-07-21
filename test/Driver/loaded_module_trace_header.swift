// RUN: rm -f %t
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_header -c %s -o- -import-objc-header %S/Inputs/loaded_module_trace_header.h > /dev/null
// RUN: %FileCheck %s < %t

// REQUIRES: objc_interop

// CHECK: {
// CHECK: "name":"loaded_module_trace_header"
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

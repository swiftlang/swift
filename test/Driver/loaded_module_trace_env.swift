// RUN: rm -f %t
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_env -c %s -o- > /dev/null
// RUN: %FileCheck %s < %t

// CHECK: {
// CHECK: "name":"loaded_module_trace_env"
// CHECK: "arch":"{{[^"]*}}"
// CHECK: "swiftmodules":[
// CHECK: "{{[^"]*(/|\\\\)}}Swift.swiftmodule"
// CHECK: "{{[^"]*(/|\\\\)}}SwiftOnoneSupport.swiftmodule"
// CHECK: ]
// CHECK: }

// RUN: rm -f %t
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_append %s -o- > /dev/null
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_append2 %s -o- > /dev/null
// RUN: %FileCheck %s < %t

// CHECK: {"name":"loaded_module_trace_append",{{.*}}]}
// CHECK-NEXT: {"name":"loaded_module_trace_append2",{{.*}}]}

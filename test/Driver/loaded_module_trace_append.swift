// RUN: rm -f %t
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_append -c %s -o- > /dev/null
// RUN: env SWIFT_LOADED_MODULE_TRACE_FILE=%t %target-build-swift -module-name loaded_module_trace_append2 -c %s -o- > /dev/null
// RUN: %FileCheck %s < %t

// CHECK: {"name":"loaded_module_trace_append",{{.*}}]}
// CHECK-NEXT: {"name":"loaded_module_trace_append2",{{.*}}]}

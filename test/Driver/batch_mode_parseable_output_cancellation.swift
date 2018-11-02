// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift
// RUN: echo 'public func main() { help_an_error_happened() }' >%t/main.swift
//
// RUN: not %swiftc_driver -enable-batch-mode -parseable-output -serialize-diagnostics -c -emit-module -module-name main -j 1 %t/file-01.swift %t/main.swift 2>&1 | %FileCheck %s
//
//      CHECK:   "kind": "signalled",
// CHECK-NEXT:   "name": "compile",
// CHECK-NEXT:   "pid": -{{[1-9][0-9]*}},
// CHECK-NEXT:   "process": {
// CHECK-NEXT:   	"real_pid": {{[1-9][0-9]*}}
// CHECK-NEXT:   },
// CHECK-NEXT:   "error-message": "{{.*}}",
// CHECK-NEXT:   "signal": 2

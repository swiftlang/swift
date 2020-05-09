// Statically link a "hello world" program
// REQUIRES: OS=linux-gnu
// REQUIRES: static_stdlib
print("hello world!")
// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -driver-print-jobs -static-stdlib -o %t/static-stdlib %s -Xlinker --no-allow-multiple-definition 2>&1| %FileCheck %s
// CHECK: {{.*}}/swift -frontend -c -primary-file {{.*}}/linker-args-order-linux.swift
// CHECK: {{.*}}/swift-autolink-extract{{.*}}
// CHECK: {{.*}}swiftrt.o /{{.*}}/linker-args-order-linux-{{[a-z0-9]+}}.o @/{{.*}}/linker-args-order-linux-{{[a-z0-9]+}}.autolink {{.*}} @{{.*}}/static-stdlib-args.lnk {{.*}} -Xlinker --no-allow-multiple-definition

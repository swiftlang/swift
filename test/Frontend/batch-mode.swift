// RUN: %empty-directory(%t)
// RUN: echo 'public func a() { }' >%t/a.swift
// RUN: echo 'public func b() { }' >%t/b.swift
// RUN: echo 'public func c() { }' >%t/c.swift
// RUN: echo 'public func main() {a(); b(); c()}' >%t/main.swift
// RUN: %target-swift-frontend -c -enable-batch-mode -module-name foo -primary-file %t/a.swift -primary-file %t/b.swift -primary-file %t/c.swift -primary-file %t/main.swift -o %t/a.o -o %t/b.o -o %t/c.o -o %t/main.o
//
// RUN: llvm-objdump -t %t/a.o | swift-demangle | %FileCheck -check-prefix=CHECK-A %s
// RUN: llvm-objdump -t %t/b.o | swift-demangle | %FileCheck -check-prefix=CHECK-B %s
// RUN: llvm-objdump -t %t/c.o | swift-demangle | %FileCheck -check-prefix=CHECK-C %s
// RUN: llvm-objdump -t %t/main.o | swift-demangle | %FileCheck -check-prefix=CHECK-MAIN %s
//
// CHECK-A: foo.a() -> ()
// CHECK-B: foo.b() -> ()
// CHECK-C: foo.c() -> ()
// CHECK-MAIN: foo.main() -> ()

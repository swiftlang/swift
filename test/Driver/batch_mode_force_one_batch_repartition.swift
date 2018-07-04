// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %swiftc_driver -enable-batch-mode -c -emit-module -module-name main -j 2 -driver-force-one-batch-repartition %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift  -###  2>%t/stderr | %FileCheck %s  -check-prefix=CHECK-COMBINED

// CHECK-COMBINED: -primary-file {{.*}}/file-01.swift
// CHECK-COMBINED-NEXT: -primary-file {{.*}}/file-02.swift
// CHECK-COMBINED-NEXT: -primary-file {{.*}}/file-03.swift
// CHECK-COMBINED-NEXT: -primary-file {{.*}}/main.swift

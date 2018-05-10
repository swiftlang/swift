// Ensure that the -### and -driver-print-jobs options work properly in batch
// mode. They should each do the same thing, so test them both.
//
// Test be sure that the output does reflect the batching, in other words
// multiple primary files. Also test to be sure that the output is on
// stdout, and NOT stderr.


// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// RUN: %swiftc_driver -enable-batch-mode -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift  -driver-print-jobs 2>%t/shouldBeEmpty1 | %FileCheck %s -check-prefix=CHECK-COMBINED
// RUN: %swiftc_driver -enable-batch-mode -c -emit-module -module-name main -j 2 %t/file-01.swift %t/file-02.swift %t/file-03.swift %t/main.swift  -###  2>%t/shouldBeEmpty2 | %FileCheck %s  -check-prefix=CHECK-COMBINED
// RUN: test -z "`cat %t/shouldBeEmpty1`"
// RUN: test -z "`cat %t/shouldBeEmpty2`"
//
// CHECK-COMBINED: -primary-file {{.*}}/file-01.swift -primary-file {{.*}}/file-02.swift {{.*}}/file-03.swift {{.*}}/main.swift

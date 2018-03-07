// XFAIL: linux

// RUN: rm -rf %t
// RUN: %target-swift-frontend -index-store-path %t/idx %s -o %t/file1.o -typecheck
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1

// RUN: %empty-directory(%t)
// RUN: touch %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -primary-file %s %t/s2.swift -o %t/file1.o -typecheck
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE1

// RUN: %empty-directory(%t)
// RUN: touch %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx %s -primary-file %t/s2.swift -o %t/file2.o -typecheck
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix=FILE2

// FILE1: file1.o-{{[A-Z0-9]*}}
// FILE1: --------
// FILE1: out-file: {{.*}}file1.o
// FILE1: DEPEND START
// FILE1: Unit | system | {{.*}}Swift.swiftmodule | | {{[0-9]*$}}
// FILE1: DEPEND END

// FILE2: file2.o-{{[A-Z0-9]*}}
// FILE2: --------
// FILE2: out-file: {{.*}}file2.o
// FILE2: DEPEND START
// FILE2: Unit | system | {{.*}}Swift.swiftmodule | | {{[0-9]*$}}
// FILE2: DEPEND END

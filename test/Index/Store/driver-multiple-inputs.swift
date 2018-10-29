// Make sure the indexing invocation through the driver works.

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swiftc_driver -index-store-path %t/idx %t/s1.swift %t/s2.swift -o %t/s1.o -index-file -index-file-path %t/s2.swift -index-ignore-system-modules -driver-filelist-threshold=0
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

// CHECK: s1.o-{{[A-Z0-9]*}}
// CHECK: --------
// CHECK: out-file: {{.*}}s1.o

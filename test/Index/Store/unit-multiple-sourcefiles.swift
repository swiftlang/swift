// XFAIL: linux

//===--- Building source files separately with a module merge at the end

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -primary-file %t/s1.swift %t/s2.swift -o %t/s1.o -c -module-name main -emit-module -emit-module-path %t/s1.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx %t/s1.swift -primary-file %t/s2.swift -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/s2.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx %t/s1.swiftmodule %t/s2.swiftmodule -emit-module -o %t/main.swiftmodule -module-name main
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

//===--- Building source files together (e.g. WMO)

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx %t/s1.swift %t/s2.swift -o %t/s1.o -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/main.swiftmodule
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

// CHECK-NOT: main.swiftmodule-{{[A-Z0-9]*}}

// CHECK: s1.o-{{[A-Z0-9]*}}
// CHECK: --------
// CHECK: out-file: {{.*}}s1.o
// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}Swift.swiftmodule | | {{[0-9]*$}}
// CHECK: DEPEND END

// CHECK: s2.o-{{[A-Z0-9]*}}
// CHECK: --------
// CHECK: out-file: {{.*}}s2.o
// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}Swift.swiftmodule | | {{[0-9]*$}}
// CHECK: DEPEND END

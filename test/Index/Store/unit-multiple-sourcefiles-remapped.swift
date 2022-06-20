//===--- Building source files separately with a module merge at the end

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file %t/s1.swift %t/s2.swift -o %t/s1.o -c -module-name main -emit-module -emit-module-path %t/s1.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swift -primary-file %t/s2.swift -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/s2.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swiftmodule %t/s2.swiftmodule -emit-module -o %t/main.swiftmodule -module-name main
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

//===--- Building source files together (e.g. WMO)

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swift %t/s2.swift -o %t/s1.o -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/main.swiftmodule
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

//===--- Building separately but with relative paths for the source file inputs

// RUN: %empty-directory(%t)
// RUN: cd %t
// RUN: touch %t/s1.swift %t/s2.swift
// RUN: %target-swift-frontend -index-store-path idx -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file s1.swift s2.swift -o s1.o -c -module-name main -emit-module -emit-module-path s1.swiftmodule
// RUN: %target-swift-frontend -index-store-path idx -file-prefix-map %t=REMAPPED_OUT_DIR s1.swift -primary-file s2.swift -o s2.o -c -module-name main -emit-module -emit-module-path s2.swiftmodule
// RUN: %target-swift-frontend -index-store-path idx -file-prefix-map %t=REMAPPED_OUT_DIR s1.swiftmodule s2.swiftmodule -emit-module -o main.swiftmodule -module-name main
// RUN: c-index-test core -print-unit idx | %FileCheck %s
// CHECK-NOT: main.swiftmodule-{{[A-Z0-9]*}}

// CHECK: s1.o-{{2LQAU7D8TZHZ8|2RHC8ZJFDYDW4}}
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s1.o
// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}Swift.swiftmodule
// CHECK: DEPEND END

// CHECK: s2.o-{{2OIL2LG8UULK6|15MCL6ZLKZKNL}}
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s2.o
// CHECK: DEPEND START
// CHECK: Unit | system | {{.*}}Swift.swiftmodule
// CHECK: DEPEND END

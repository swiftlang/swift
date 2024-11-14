//===--- Building source files separately with a module merge at the end

// RUN: %empty-directory(%t)
// RUN: touch %t/s1.swift %t/s2.swift

// RUN: echo "-----separate-----" > %t/units.out
// RUN: %target-swift-frontend -index-store-path %t/idx1 -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file %t/s1.swift %t/s2.swift -o %t/s1.o -c -module-name main -emit-module -emit-module-path %t/s1.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx1 -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swift -primary-file %t/s2.swift -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/s2.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx1 -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swiftmodule %t/s2.swiftmodule -emit-module -o %t/main.swiftmodule -module-name main
// RUN: c-index-test core -print-unit %t/idx1 >> %t/units.out

//===--- Building source files together (e.g. WMO)

// RUN: echo "-----together-----" >> %t/units.out
// RUN: %target-swift-frontend -index-store-path %t/idx2 -file-prefix-map %t=REMAPPED_OUT_DIR %t/s1.swift %t/s2.swift -o %t/s1.o -o %t/s2.o -c -module-name main -emit-module -emit-module-path %t/main.swiftmodule
// RUN: c-index-test core -print-unit %t/idx2 >> %t/units.out

//===--- Building separately but with relative paths for the source file inputs

// RUN: cd %t
// RUN: echo "-----together-relative-----" >> %t/units.out
// RUN: %target-swift-frontend -index-store-path idx3 -file-prefix-map %t=REMAPPED_OUT_DIR -primary-file s1.swift s2.swift -o s1.o -c -module-name main -emit-module -emit-module-path s1.swiftmodule
// RUN: %target-swift-frontend -index-store-path idx3 -file-prefix-map %t=REMAPPED_OUT_DIR s1.swift -primary-file s2.swift -o s2.o -c -module-name main -emit-module -emit-module-path s2.swiftmodule
// RUN: %target-swift-frontend -index-store-path idx3 -file-prefix-map %t=REMAPPED_OUT_DIR s1.swiftmodule s2.swiftmodule -emit-module -o main.swiftmodule -module-name main
// RUN: c-index-test core -print-unit idx3 >> %t/units.out

// RUN: %FileCheck %s --dump-input-filter all < %t/units.out

// CHECK: -----separate-----
// CHECK: s1.o-[[S1_HASH:.*$]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s1.o
// CHECK: s2.o-[[S2_HASH:.*$]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s2.o

// CHECK: -----together-----
// CHECK: s1.o-[[S1_HASH]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s1.o
// CHECK: s2.o-[[S2_HASH]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s2.o

// CHECK: -----together-relative-----
// CHECK: s1.o-[[S1_HASH]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s1.o
// CHECK: s2.o-[[S2_HASH]]
// CHECK: --------
// CHECK: out-file: REMAPPED_OUT_DIR{{/|\\}}s2.o

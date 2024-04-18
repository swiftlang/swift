// Check interface produced for the standard library.
//
// REQUIRES: long_test
// REQUIRES: nonexecutable_test
//

// RUN: %target-swift-ide-test -print-module -module-group "Array" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Bridging" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Floats" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "HashedCollections" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Integers" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Protocols" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Reflection" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Sequences+Collections" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-COLLECTION-GROUP
// RUN: %target-swift-ide-test -print-module -module-group "SIMD" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "String" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Unicode" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Unsafe" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Misc" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD

// CHECK-FREQUENT-WORD: ///
// CHECK-FREQUENT-WORD-NOT: protocol Collection
// CHECK-COLLECTION-GROUP: protocol Collection

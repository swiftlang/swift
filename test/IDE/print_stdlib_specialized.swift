// Check interface produced for the standard library.
//
// REQUIRES: long_test
// REQUIRES: nonexecutable_test
//

// RUN: %target-swift-ide-test -print-module -module-group "Pointer" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "C" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Protocols" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Optional" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Collection/Lazy Views" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Math" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Math/Floating" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Math/Integers" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Reflection" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Misc" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Collection" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-COLLECTION-GROUP
// RUN: %target-swift-ide-test -print-module -module-group "Bool" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Assert" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "String" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Collection/Array" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Collection/Type-erased" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD
// RUN: %target-swift-ide-test -print-module -module-group "Collection/HashedCollections" -synthesize-extension -module-to-print=Swift -source-filename %s -print-interface | %FileCheck %s -check-prefix=CHECK-FREQUENT-WORD

// CHECK-FREQUENT-WORD: ///
// CHECK-FREQUENT-WORD-NOT: where Slice<Dictionary<Key, Value>> == Slice<Self>
// CHECK-COLLECTION-GROUP: extension MutableCollection

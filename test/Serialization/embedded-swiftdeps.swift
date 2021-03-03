// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name EmbeddedIncremental -o %t/EmbeddedIncremental~partial.swiftmodule -primary-file %s
// RUN: %target-swift-frontend -emit-module -merge-modules -module-name EmbeddedIncremental -o %t/EmbeddedIncremental.swiftmodule %t/EmbeddedIncremental~partial.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/EmbeddedIncremental.swiftmodule | %FileCheck %s --dump-input=always

// RUN: %target-swift-frontend -emit-module -disable-incremental-imports -module-name NoEmbeddedIncremental -o %t/NoEmbeddedIncremental~partial.swiftmodule -primary-file %s
// RUN: %target-swift-frontend -emit-module -merge-modules -disable-incremental-imports -module-name NoEmbeddedIncremental -o %t/NoEmbeddedIncremental.swiftmodule %t/NoEmbeddedIncremental~partial.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/NoEmbeddedIncremental.swiftmodule | %FileCheck %s --dump-input=always --check-prefix=NO-INCREMENTAL-IMPORTS


public struct AnyWindows {}

// CHECK: <INCREMENTAL_INFORMATION_BLOCK
// CHECK: </INCREMENTAL_INFORMATION_BLOCK>

// NO-INCREMENTAL-IMPORTS-NOT: <INCREMENTAL_INFORMATION_BLOCK

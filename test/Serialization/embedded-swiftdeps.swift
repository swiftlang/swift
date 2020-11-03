// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name EmbeddedIncremental -o %t/EmbeddedIncremental~partial.swiftmodule -primary-file %s
// RUN: %target-swift-frontend -enable-experimental-cross-module-incremental-build -emit-module -module-name EmbeddedIncremental -o %t/EmbeddedIncremental.swiftmodule %t/EmbeddedIncremental~partial.swiftmodule
// RUN: llvm-bcanalyzer -dump %t/EmbeddedIncremental.swiftmodule | %FileCheck %s --dump-input=always

public struct AnyWindows {}

// CHECK: <INCREMENTAL_INFORMATION_BLOCK
// CHECK: </INCREMENTAL_INFORMATION_BLOCK>

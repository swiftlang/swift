// Adding a private stored property to a public struct in a non-resilient
// module changes layout but not the public interface. The module
// fingerprint must still change so the driver reads the dependency
// graph and invalidates dependents.

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/incremental-imports-layout/Erased-before.swift %t/Erased.swift

// RUN: %target-swift-frontend -emit-module -module-name IncrementalImportsLayout -experimental-skip-non-inlinable-function-bodies-without-types -o %t/IncrementalImportsLayout.swiftmodule %t/Erased.swift

// RUN: %llvm-bcanalyzer -dump %t/IncrementalImportsLayout.swiftmodule | %FileCheck %s --check-prefix=BEFORE

// BEFORE-LABEL: SOURCE_FILE_DEP_GRAPH_NODE abbrevid=5 op0=6
// BEFORE-NEXT: FINGERPRINT_NODE {{.*}} blob data = '400d6f14974cc4b2319972949ae59e77'

// RUN: cp %S/Inputs/incremental-imports-layout/Erased-after.swift %t/Erased.swift
// RUN: %target-swift-frontend -emit-module -module-name IncrementalImportsLayout -experimental-skip-non-inlinable-function-bodies-without-types -o %t/IncrementalImportsLayout.swiftmodule %t/Erased.swift

// RUN: %llvm-bcanalyzer -dump %t/IncrementalImportsLayout.swiftmodule | %FileCheck %s --check-prefix=AFTER

// AFTER-LABEL: SOURCE_FILE_DEP_GRAPH_NODE abbrevid=5 op0=6
// AFTER-NEXT: FINGERPRINT_NODE {{.*}} blob data = '769ff2954a40be6aa0686156fb89bed0'

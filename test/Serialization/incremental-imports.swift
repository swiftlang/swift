// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/incremental-imports/* %t
// RUN: cp %t/A-before.swift %t/A.swift


// RUN: %target-swift-frontend -emit-module -module-name IncrementalImports -o %t/IncrementalImports~A.swiftmodule -primary-file %t/A.swift
// RUN: %target-swift-frontend -merge-modules -emit-module -module-name IncrementalImports -o %t/IncrementalImports.swiftmodule %t/IncrementalImports~A.swiftmodule

// RUN: llvm-bcanalyzer -dump %t/IncrementalImports.swiftmodule | %FileCheck %s --check-prefix=INCREMENTAL-IMPORTS-BASELINE

// INCREMENTAL-IMPORTS-BASELINE-LABEL: <INCREMENTAL_INFORMATION_BLOCK
// Test for the fingerprint for the class
// INCREMENTAL-IMPORTS-BASELINE-DAG: blob data = '7de0a38047d74950f4f2ced447ab0242'
// And for its member
// INCREMENTAL-IMPORTS-BASELINE-DAG: blob data = 'e79735e7b1e8c65831c70766207a75f3'
// INCREMENTAL-IMPORTS-BASELINE-LABEL: </INCREMENTAL_INFORMATION_BLOCK>

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/incremental-imports/* %t
// RUN: cp %t/A-after.swift %t/A.swift
// RUN: %target-swift-frontend -emit-module -module-name IncrementalImports -o %t/IncrementalImports~A.swiftmodule -primary-file %t/A.swift
// RUN: %target-swift-frontend -merge-modules -emit-module -module-name IncrementalImports -o %t/IncrementalImports.swiftmodule %t/IncrementalImports~A.swiftmodule

// RUN: llvm-bcanalyzer -dump %t/IncrementalImports.swiftmodule | %FileCheck %s --check-prefix=INCREMENTAL-IMPORTS-MUTATION

// INCREMENTAL-IMPORTS-MUTATION-LABEL: <INCREMENTAL_INFORMATION_BLOCK
// Make sure the fingerprint for the class doesn't change
// INCREMENTAL-IMPORTS-MUTATION-DAG: blob data = '7de0a38047d74950f4f2ced447ab0242'
// Make sure the fingerprint for the member changes
// INCREMENTAL-IMPORTS-MUTATION-DAG: blob data = '99bb01bb4d9177dc6f902d1f2326caad'
// INCREMENTAL-IMPORTS-MUTATION-LABEL: </INCREMENTAL_INFORMATION_BLOCK>

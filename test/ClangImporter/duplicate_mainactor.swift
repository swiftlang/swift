// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk)  -import-objc-header %S/Inputs/DoubleMainActor.h -emit-module -module-name use %s 2> %t/stderr.txt
// RUN: %FileCheck -input-file %t/stderr.txt %s

// REQUIRES: concurrency
// REQUIRES: objc_interop

// CHECK: DoubleMainActor.h:{{[0-9]+}}:{{[0-9]+}}: warning: this attribute for global actor '@MainActor' is invalid; the declaration already has attribute for global actor '@UIActor' [#ClangDeclarationImport]

protocol P : DoubleMainActor {}

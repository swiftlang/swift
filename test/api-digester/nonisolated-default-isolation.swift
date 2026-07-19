// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// Scenario A: default (nonisolated) module isolation. Adding an explicit
// `nonisolated` keyword is a semantic no-op and must NOT be reported as a
// breaking change.
// RUN: echo "public struct S {}"             > %t/Foo-1.swift
// RUN: echo "public nonisolated struct S {}" > %t/Foo-2.swift
// RUN: %target-swift-frontend -emit-module %t/Foo-1.swift -module-name Foo -o %t/Foo1.swiftmodule -emit-abi-descriptor-path %t/Foo1.json
// RUN: %target-swift-frontend -emit-module %t/Foo-2.swift -module-name Foo -o %t/Foo2.swiftmodule -emit-abi-descriptor-path %t/Foo2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Foo1.json -input-paths %t/Foo2.json -o %t/result-nonisolated.txt
// RUN: %FileCheck %s --check-prefix=NONISO --allow-empty < %t/result-nonisolated.txt

// NONISO-NOT: is now with nonisolated

// Scenario B: `-default-isolation MainActor`. An unannotated decl is implicitly
// `@MainActor`, so adding `nonisolated` genuinely changes isolation and must
// STILL be reported as a breaking change.
// RUN: echo "public struct T {}"             > %t/Bar-1.swift
// RUN: echo "public nonisolated struct T {}" > %t/Bar-2.swift
// RUN: %target-swift-frontend -emit-module %t/Bar-1.swift -module-name Bar -default-isolation MainActor -o %t/Bar1.swiftmodule -emit-abi-descriptor-path %t/Bar1.json
// RUN: %target-swift-frontend -emit-module %t/Bar-2.swift -module-name Bar -default-isolation MainActor -o %t/Bar2.swiftmodule -emit-abi-descriptor-path %t/Bar2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t/Bar1.json -input-paths %t/Bar2.json -o %t/result-mainactor.txt
// RUN: %FileCheck %s --check-prefix=MAINACTOR < %t/result-mainactor.txt

// MAINACTOR: is now with nonisolated

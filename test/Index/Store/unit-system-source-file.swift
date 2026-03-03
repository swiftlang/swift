// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK)
// RUN: cp %s %t/SDK/FakeSystemModule.swift

// When the source file is in the SDK, consider it a system module.
//
// RUN: %target-swift-frontend -c -o %t/FakeSystemModule.o -index-store-path %t/idx -sdk %t/SDK %t/SDK/FakeSystemModule.swift
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s
//
// CHECK: FakeSystemModule.o-{{[A-Z0-9]*}}
// CHECK: --------
// CHECK: is-system: 1

// RUN: %target-swift-frontend -c -o %t/FakeSystemModule.o -index-store-path %t/idx %t/SDK/FakeSystemModule.swift
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s -check-prefix NO-SDK
//
// NO-SDK: FakeSystemModule.o-{{[A-Z0-9]*}}
// NO-SDK: --------
// NO-SDK: is-system: 0

func someFunc() {}

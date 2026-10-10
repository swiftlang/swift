// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/SDK

// RUN: %target-swift-frontend -emit-module -module-name ProviderModule -swift-version 5 %t/ProviderModule.swift -o %t/SDK/ProviderModule.swiftmodule

// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name Client -sdk %t/SDK -I %t/SDK -index-system-modules -index-ignore-stdlib -index-store-path %t/idx5 %t/client.swift -o %t/client.o
// RUN: %target-swift-frontend -version > %t/units5.out
// RUN: c-index-test core -print-unit %t/idx5 >> %t/units5.out
// RUN: %FileCheck %s < %t/units5.out

// RUN: %target-swift-frontend -typecheck -swift-version 6 -module-name Client -sdk %t/SDK -I %t/SDK -index-system-modules -index-ignore-stdlib -index-store-path %t/idx6 %t/client.swift -o %t/client.o
// RUN: %target-swift-frontend -version > %t/units6.out
// RUN: c-index-test core -print-unit %t/idx6 >> %t/units6.out
// RUN: %FileCheck %s < %t/units6.out

// CHECK: [[COMPILER_VERSION:.*Swift version.*]]
// CHECK: {{^}}ProviderModule.swiftmodule-{{[A-Z0-9]+$}}
// CHECK-NEXT: --------
// CHECK-NEXT: provider: swift-[[COMPILER_VERSION]]{{$}}
// CHECK-NEXT: is-system: 1
// CHECK-NEXT: is-module: 1

// CHECK: {{^}}client.o-{{[A-Z0-9]+$}}
// CHECK-NEXT: --------
// CHECK-NEXT: provider: swift-[[COMPILER_VERSION]]{{$}}
// CHECK-NEXT: is-system: 0
// CHECK-NEXT: is-module: 0

//--- ProviderModule.swift
public struct IndexedType {}

//--- client.swift
import ProviderModule

func use(_ value: IndexedType) {}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/SDK

// Build a serialized module inside a mock SDK so it is indexed as a system
// module when imported. Its unique filename identifies its unit in the dump.
// RUN: %target-swift-frontend -emit-module -module-name ProviderModule -swift-version 5 %t/ProviderModule.swift -o %t/SDK/ProviderModule.swiftmodule

// Both source-file and serialized-module units must identify the compiler,
// independently of the selected language compatibility mode. Reading the whole
// store with c-index-test exercises the public index-store reader API.
// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name Client -sdk %t/SDK -I %t/SDK -index-system-modules -index-ignore-stdlib -index-store-path %t/idx5 %t/client.swift -o %t/client.o
// RUN: %target-swift-frontend -version > %t/units5.out
// RUN: c-index-test core -print-unit %t/idx5 >> %t/units5.out
// RUN: %FileCheck %s --check-prefix=MODULE < %t/units5.out
// RUN: %FileCheck %s --check-prefix=SOURCE < %t/units5.out

// RUN: %target-swift-frontend -typecheck -swift-version 6 -module-name Client -sdk %t/SDK -I %t/SDK -index-system-modules -index-ignore-stdlib -index-store-path %t/idx6 %t/client.swift -o %t/client.o
// RUN: %target-swift-frontend -version > %t/units6.out
// RUN: c-index-test core -print-unit %t/idx6 >> %t/units6.out
// RUN: %FileCheck %s --check-prefix=MODULE < %t/units6.out
// RUN: %FileCheck %s --check-prefix=SOURCE < %t/units6.out

// c-index-test displays the separate provider identifier and version joined by
// a hyphen; the identifier itself remains "swift".
// MODULE: [[COMPILER_VERSION:.*Swift version.*]]
// MODULE: {{^}}ProviderModule.swiftmodule-{{[A-Z0-9]+$}}
// MODULE-NEXT: --------
// MODULE-NEXT: provider: swift-[[COMPILER_VERSION]]{{$}}
// MODULE-NEXT: is-system: 1
// MODULE-NEXT: is-module: 1
// MODULE-NEXT: module-name: ProviderModule

// SOURCE: [[COMPILER_VERSION:.*Swift version.*]]
// SOURCE: {{^}}client.o-{{[A-Z0-9]+$}}
// SOURCE-NEXT: --------
// SOURCE-NEXT: provider: swift-[[COMPILER_VERSION]]{{$}}
// SOURCE-NEXT: is-system: 0
// SOURCE-NEXT: is-module: 0
// SOURCE-NEXT: module-name: Client

//--- ProviderModule.swift
public struct IndexedType {}

//--- client.swift
import ProviderModule

func use(_ value: IndexedType) {}

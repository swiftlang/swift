// RUN: %empty-directory(%t)
//
// Test will build a module TestModule that depends on OtherModule and LeafModule (built from other.swift and leaf.swift).
//
// RUN: echo 'public func LeafFunc() -> Int { return 10; }' >%t/leaf.swift
//
// RUN: echo 'import LeafModule' >%t/other.swift
// RUN: echo 'public func OtherFunc() -> Int { return LeafFunc(); }' >>%t/other.swift
//
// Phase 1: build LeafModule into a .swiftinterface file with -swift-version 4:
//
// RUN: %target-swift-frontend -swift-version 4 -I %t -module-cache-path %t/modulecache -emit-module-interface-path %t/LeafModule.swiftinterface -module-name LeafModule %t/leaf.swift -typecheck
//
// Phase 2: build OtherModule into a .swiftinterface file with -swift-version 4.2:
//
// RUN: %target-swift-frontend -swift-version 4.2 -I %t -module-cache-path %t/modulecache -emit-module-interface-path %t/OtherModule.swiftinterface -module-name OtherModule %t/other.swift -typecheck
//
// Phase 3: build TestModule in -swift-version 5 and import both of these:
//
// RUN: %target-swift-frontend -swift-version 5 -I %t -module-cache-path %t/modulecache -module-name TestModule %s -typecheck
//
// Phase 4: make sure we only compiled LeafModule and OtherModule one time:
//
// RUN: ls %t/modulecache | grep -c 'LeafModule-.*.swiftmodule' | %FileCheck %s
// RUN: ls %t/modulecache | grep -c 'OtherModule-.*.swiftmodule' | %FileCheck %s
// CHECK: 1

import LeafModule
import OtherModule

// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)
// RUN: %empty-directory(%t/PrebuiltCache)

// 1. Create a dummy module
// RUN: echo 'public func publicFunction() {}' > %t/TestModule.swift

// 2. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5

// 3. Create an empty .swiftmodule, which will force recompiling from the interface
// RUN: touch %t/Build/TestModule.swiftmodule

// 4. Try to import the malformed compiled module
// RUN: %target-swift-frontend -typecheck -verify %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

import TestModule // expected-remark {{rebuilding module 'TestModule' from interface}}
// expected-note @-1 {{is out of date}}
// expected-note @-2 {{malformed}}
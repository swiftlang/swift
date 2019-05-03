// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)

// 1. Create a dummy module
// RUN: echo 'public func publicFunction() {}' > %t/TestModule.swift

// 2. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5

// 3. Try to import the interface, which will pass and create a cached module
// RUN: %target-swift-frontend -typecheck %s -I %t/Build -module-cache-path %t/ModuleCache

// 4. Touch the interface so the cached module is no longer up-to-date
// RUN: touch %t/Build/TestModule.swiftinterface

// 5. Try to import the now out-of-date cached module
// RUN: %target-swift-frontend -typecheck -verify %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache -Xllvm -debug-only=textual-module-interface

import TestModule // expected-remark {{rebuilding module 'TestModule' from interface}}
// expected-note @-1 {{cached module is out of date}}
// expected-note @-2 {{dependency is out of date}}

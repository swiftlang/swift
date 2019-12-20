// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)

// 1. Create a dummy module
// RUN: echo 'public func publicFunction() {}' > %t/TestModule.swift

// 2. Create both an interface and a compiled module for it
// RUN: %target-swift-frontend -emit-module -o %t/Build/TestModule.swiftmodule %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5 -module-name TestModule

// 3. Make the compiled module unreadable so it gets added as a dependency but is not used
// RUN: mv %t/Build/TestModule.swiftmodule %t/Build/TestModule.swiftmodule.moved-aside
// RUN: echo 'this is unreadable' > %t/Build/TestModule.swiftmodule

// 4. Try to import the module, which will create a cached module that depends on both the .swiftmodule and .swiftinterface
// RUN: %target-swift-frontend -typecheck %s -I %t/Build -module-cache-path %t/ModuleCache

// 5. Remove the .swiftmodule, which will result in a missing dependency note
// RUN: rm %t/Build/TestModule.swiftmodule

// 6. Rebuild with remarks enabled, make sure the missing dependency note is emitted
// RUN: %target-swift-frontend -typecheck -verify %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

// 7. Put the module back, make the interface unreadable, and make sure the compiler succeeds
// RUN: mv %t/Build/TestModule.swiftmodule.moved-aside %t/Build/TestModule.swiftmodule
// RUN: mv %t/Build/TestModule.swiftinterface %t/Build/TestModule.swiftinterface.moved-aside
// RUN: echo 'this is unreadable' > %t/Build/TestModule.swiftinterface
// RUN: %target-swift-frontend -typecheck %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

import TestModule // expected-remark {{rebuilding module 'TestModule' from interface}}
// expected-note @-1 {{cached module is out of date}}
// expected-note @-2 {{dependency is missing}}

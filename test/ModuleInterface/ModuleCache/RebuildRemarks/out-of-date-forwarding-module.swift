// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)
// RUN: %empty-directory(%t/PrebuiltCache)

// 1. Create a dummy module
// RUN: echo 'public func publicFunction() {}' > %t/TestModule.swift

// 2. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5

// 3. Build the .swiftinterface to a .swiftmodule in the prebuilt cache, which will have a dependency on the interface
// RUN: %target-swift-frontend -compile-module-from-interface %t/Build/TestModule.swiftinterface -o %t/PrebuiltCache/TestModule.swiftmodule

// 5. Try to import the prebuilt module (this should pass)
// RUN: %target-swift-frontend -typecheck %s -I %t/Build -sdk %t -prebuilt-module-cache-path %t/PrebuiltCache -module-cache-path %t/ModuleCache

// 6. Make sure we installed a forwarding module in the cache
// RUN: %{python} %S/../Inputs/check-is-forwarding-module.py %t/ModuleCache/TestModule-*.swiftmodule

// 7. Modify the interface so the forwarding module and prebuilt modules are no longer up-to-date
// RUN: echo ' ' >> %t/Build/TestModule.swiftinterface

// 8. Try to import the now out-of-date forwarding module, which will fail.
//    It will also fail to load the prebuilt module after the forwarding module
//    is rejected, meaning we'll get a second set of notes about the prebuilt module.
// RUN: %target-swift-frontend -typecheck -verify %s -I %t/Build -Rmodule-interface-rebuild -sdk %t -prebuilt-module-cache-path %t/PrebuiltCache -module-cache-path %t/ModuleCache

import TestModule // expected-remark {{rebuilding module 'TestModule' from interface}}
// expected-note @-1 {{forwarding module is out of date}}
// expected-note @-2 {{dependency is out of date}}
// expected-note @-3 {{prebuilt module is out of date}}
// expected-note @-4 {{dependency is out of date}}

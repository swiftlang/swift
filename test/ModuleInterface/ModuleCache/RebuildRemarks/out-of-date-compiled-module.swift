// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)

// 1. Create a dummy module
// RUN: echo 'public func publicFunction() {}' > %t/TestModule.swift

// 2. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5

// 3. Build the .swiftinterface to a .swiftmodule, which will have a dependency on the interface
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/Build/TestModule.swiftmodule %t/Build/TestModule.swiftinterface

// 3a. Make sure the test works on a fast machine
// RUN: sleep 1

// 4. Touch the interface so the module is no longer up-to-date
// RUN: %{python} %S/../Inputs/make-old.py %t/Build/TestModule.swiftinterface

// 5. Try to import the out-of-date compiled module
// RUN: %target-swift-frontend -typecheck -verify %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache

import TestModule // expected-remark {{rebuilding module 'TestModule' from interface}}
// expected-note @-1 {{compiled module is out of date}}
// expected-note @-2 {{dependency is out of date}}

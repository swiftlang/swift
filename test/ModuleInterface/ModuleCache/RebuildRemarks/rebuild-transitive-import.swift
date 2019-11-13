// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Build)
// RUN: %empty-directory(%t/ModuleCache)

// 1. Create a module called InnerModule, and put its interface into the build dir.
// RUN: echo 'public func inInnerModule() {}' | %target-swift-frontend - -typecheck -emit-module-interface-path %t/Build/InnerModule.swiftinterface -enable-library-evolution -swift-version 5 -module-name InnerModule

// 2. Build the .swiftinterface to a .swiftmodule, which will have a dependency on the interface
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/Build/InnerModule.swiftmodule %t/Build/InnerModule.swiftinterface

// 3. Touch the interface so the module becomes out of date.
// RUN: %{python} %S/../Inputs/make-old.py %t/Build/InnerModule.swiftinterface

// 4. Create a module called OuterModule that imports InnerModule, and put its interface into the build dir.
// RUN: echo 'import InnerModule' | %target-swift-frontend - -emit-module -o %t/Build/OuterModule.swiftmodule -module-name OuterModule -I %t/Build

// 5. Build this file, and expect that InnerModule is out of date
// RUN: %target-swift-frontend -typecheck %s -I %t/Build -Rmodule-interface-rebuild -module-cache-path %t/ModuleCache 2>&1 | %FileCheck %s

import OuterModule

// CHECK: rebuilding module 'InnerModule' from interface '{{.*}}{{[/\\]}}Build{{[/\\]}}InnerModule.swiftinterface'
// CHECK: compiled module is out of date: '{{.*}}{{[/\\]}}Build{{[/\\]}}InnerModule.swiftmodule'
// CHECK: dependency is out of date: '{{.*}}{{[/\\]}}Build{{[/\\]}}InnerModule.swiftinterface'

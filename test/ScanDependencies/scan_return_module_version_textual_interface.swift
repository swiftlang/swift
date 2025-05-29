// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/DependencyModules)

// Emit a textual module dependency
// RUN: %target-swift-frontend -emit-module -emit-module-interface-path %t/DependencyModules/TextualFoo.swiftinterface -module-cache-path %t/clang-module-cache -module-name TextualFoo %s -D TEXTUAL_FOO -user-module-version 12.3.4

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/DependencyModules/Foo.swiftmodule -module-cache-path %t/clang-module-cache -module-name Foo %s -D FOO -user-module-version 42.3.3

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %t/DependencyModules/ -module-name main

// Check the contents of the JSON output
// RUN: %FileCheck -check-prefix CHECK %s < %t/deps.json

#if FOO

public func foo() {}

#elseif TEXTUAL_FOO


#else
import Foo
import TextualFoo

#endif

// CHECK:  "userModuleVersion": "12.3.4"
// CHECK:  "userModuleVersion": "42.3.3.0"

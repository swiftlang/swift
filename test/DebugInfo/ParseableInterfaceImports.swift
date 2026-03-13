// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-typecheck %S/advanced.swift \
// RUN:    -emit-module-interface-path %t/advanced.swiftinterface
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    -sdk %t | %FileCheck %s --check-prefix=SDK

import advanced

// CHECK: !DIModule(scope: null, name: "advanced",
// CHECK-SAME:      includePath: "
// CHECK-SAME:      advanced.swiftinterface"

// Even if the module interface is in the SDK, we still return the path
// to the swiftinterface.
// SDK:   !DIModule(scope: null, name: "advanced",
// SDK-SAME:        includePath: "
// SDK-SAME:        advanced{{.*}}.swiftinterface"

func markUsed<T>(_ t: T) {}
markUsed(advanced.foo(1, 2))


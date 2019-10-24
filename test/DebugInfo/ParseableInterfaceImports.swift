// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-typecheck %S/basic.swift \
// RUN:    -emit-module-interface-path %t/basic.swiftinterface
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    -sdk %t | %FileCheck %s --check-prefix=SDK

import basic

// CHECK: !DIModule(scope: null, name: "basic", includePath: "
// CHECK-SAME:      basic.swiftinterface"

// Even if the module interface is in the SDK, we still return the path
// to the swiftinterface.
// SDK:   !DIModule(scope: null, name: "basic", includePath: "
// SDK-SAME:        basic{{.*}}.swiftinterface"

func markUsed<T>(_ t: T) {}
markUsed(basic.foo(1, 2))


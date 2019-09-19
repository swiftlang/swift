// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-typecheck %S/basic.swift \
// RUN:    -emit-module-interface-path %t/basic.swiftinterface
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -module-name Foo %s -I %t -g -o - \
// RUN:    -sdk %t | %FileCheck %s --check-prefix=SDK
// RUN: %target-swift-frontend -c -module-name Foo %s -I %t -g -o - \
// RUN:    | %llvm-dwarfdump - \
// RUN:    | %FileCheck %s --check-prefix=DWARF

import basic

// CHECK: !DIModule(scope: null, name: "basic", includePath: "
// CHECK-SAME:      basic.swiftinterface"

// We don't record any module interfaces from the SDK.
// They're in the SDK after all.
// SDK:   !DIModule(scope: null, name: "basic", includePath: "
// SDK-SAME:        basic{{.*}}.swiftmodule"

// DWARF: DW_AT_LLVM_include_path{{.*}}basic.swiftinterface

func markUsed<T>(_ t: T) {}
markUsed(basic.foo(1, 2))


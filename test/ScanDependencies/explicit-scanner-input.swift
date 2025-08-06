// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Inputs/Foo.swiftmodule)
// RUN: split-file %s %t

// Step 1: build swift interface and swift module side by side
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Inputs/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo

// Step 2: scan dependency should give us the binary module we specify with 'swift-module-file'
// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -o %t/deps.json -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix=CHECK-INPUT

// Step 3: ensure that if multiple inputs for the same module are specified then a warning is emitted and the latter is preferred
// RUN: echo "Gibberish" > %t/Inputs/Foo.swiftmodule/NotAModule.swiftmodule
// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -o %t/deps.json -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/NotAModule.swiftmodule -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name -diagnostic-style llvm 2>&1 | %FileCheck %s -check-prefix=CHECK-WARN-MULTIPLE
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix=CHECK-INPUT

// Step 4: verify that the usual invalid module candidate diagnostics apply
// RUN: echo "Not Really a module" > %t/Inputs/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -o %t/deps.json  -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name -diagnostic-style llvm 2>&1 | %FileCheck %s -check-prefix=CHECK-INVALID-MODULE-DIAG

// CHECK-INPUT: "swiftPrebuiltExternal": "Foo"
// CHECK-WARN-MULTIPLE: warning: multiple Swift module file inputs with identifier "Foo": replacing '{{.*}}NotAModule.swiftmodule'

// CHECK-INVALID-MODULE-DIAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}{{.*}}.swiftmodule' is incompatible with this Swift compiler: malformed
// CHECK-INVALID-MODULE-DIAG: error: Unable to find module dependency: 'Foo'
// CHECK-INVALID-MODULE-DIAG: note: a dependency of main module 'deps'

//--- Foo.swift
public func foo() {}

//--- test.swift
import Foo

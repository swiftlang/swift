// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs/Foo.swiftmodule)
// RUN: touch %t/inputs/Foo.swiftmodule/i387.swiftmodule
// RUN: touch %t/inputs/Foo.swiftmodule/ppc65.swiftmodule

// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/inputs -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s

// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}ppc65.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}i387.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK: error: unable to resolve module dependency: 'Foo'

// A later search path holds a module for the current target, so the
// directory with only other-target variants is not diagnosed.
// RUN: %empty-directory(%t/valid/Foo.swiftmodule)
// RUN: echo "public func foo() {}" > %t/Foo.swift
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -emit-module-path %t/valid/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/inputs -I %t/valid -diagnostic-style llvm -scanner-module-validation -warnings-as-errors 2>&1 | %FileCheck %s -check-prefix=RESOLVED --allow-empty
// RUN: %FileCheck %s -check-prefix=RESOLVED-DEPS < %t/deps.json

// RESOLVED-NOT: warning:
// RESOLVED-NOT: error:
// RESOLVED-DEPS: "compiledModulePath": "{{.*}}valid{{.*}}Foo.swiftmodule{{.*}}.swiftmodule"

// A candidate rejected for another reason is still diagnosed when the
// module is found on a later search path.
// RUN: %empty-directory(%t/malformed/Foo.swiftmodule)
// RUN: echo "Not Really a module" > %t/malformed/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/inputs -I %t/malformed -I %t/valid -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s -check-prefix=MALFORMED

// MALFORMED-NOT: built for incompatible target
// MALFORMED: warning: module file '{{.*}}malformed{{/|\\}}Foo.swiftmodule{{/|\\}}{{.*}}.swiftmodule' is incompatible with this Swift compiler: malformed
// MALFORMED-NOT: warning:
// MALFORMED-NOT: error:

import Foo

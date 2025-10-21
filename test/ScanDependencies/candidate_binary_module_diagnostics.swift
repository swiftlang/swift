// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/moduleInputs)
// RUN: %empty-directory(%t/moduleInputs2)
// RUN: split-file %s %t

// RUN: echo "Not Really a module" >> %t/moduleInputs/FooBar.swiftmodule
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/main.swift -o %t/deps.json -I %t/moduleInputs -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s -check-prefix=ERROR

// ERROR: error: unable to resolve Swift module dependency to a compatible module: 'FooBar'
// ERROR: note: found incompatible module '{{.*}}{{/|\\}}moduleInputs{{/|\\}}FooBar.swiftmodule': malformed

// RUN: %target-swift-frontend -emit-module %t/FooBar.swift -emit-module-path %t/moduleInputs2/FooBar.swiftmodule -module-name FooBar
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/main.swift -o %t/deps.json -I %t/moduleInputs -I %t/moduleInputs2 -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s -check-prefix=WARNING

// WARNING: warning: module file '{{.*}}{{/|\\}}moduleInputs{{/|\\}}FooBar.swiftmodule' is incompatible with this Swift compiler: malformed

//--- main.swift
import FooBar

//--- FooBar.swift
public func fooBar() {}

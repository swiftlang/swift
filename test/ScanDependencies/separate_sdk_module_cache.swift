// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/sdk-module-cache)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/FooBar.framework/Modules/FooBar.swiftmodule)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/Baz.framework/Modules)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/Baz.framework/Headers)

// RUN: split-file %s %t
// RUN: cp %t/FooBar.swiftinterface %t/mock.sdk/System/Library/Frameworks/FooBar.framework/Modules/FooBar.swiftmodule/%target-swiftinterface-name
// RUN: cp %t/module.modulemap %t/mock.sdk/System/Library/Frameworks/Baz.framework/Modules/module.modulemap
// RUN: cp %t/Baz.h %t/mock.sdk/System/Library/Frameworks/Baz.framework/Headers/Baz.h

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %S/Inputs/Swift -I %S/Inputs/CHeaders -sdk %t/mock.sdk -sdk-module-cache-path %t/sdk-module-cache
// RUN: %validate-json %t/deps.json | %FileCheck %s

//--- module.modulemap
framework module Baz {
  header "Baz.h"
  export *
}

//--- Baz.h
void funcBaz(void);

//--- FooBar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FooBar -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -user-module-version 1.0
public func foo() {}

//--- test.swift
import E
import X
import FooBar
import Baz

// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}module-cache{{/|\\}}E-{{.*}}.swiftmodule"
// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}module-cache{{/|\\}}X-{{.*}}.pcm"
// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}sdk-module-cache{{/|\\}}FooBar-{{.*}}.swiftmodule"
// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}sdk-module-cache{{/|\\}}Baz-{{.*}}.pcm

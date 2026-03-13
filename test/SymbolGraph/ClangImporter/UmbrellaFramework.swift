// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/frameworks/UmbrellaFramework.framework/Modules/UmbrellaFramework.swiftmodule
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t/frameworks/UmbrellaFramework.framework/Modules/UmbrellaFramework.swiftmodule/%target-swiftmodule-name -import-underlying-module -F %t/frameworks -module-name UmbrellaFramework -disable-objc-attr-requires-foundation-module %t/UmbrellaSwift.swift -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name UmbrellaFramework -F %t/frameworks -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/UmbrellaFramework.symbols.json

// Ensure that Clang modules with umbrella headers and 'module * { export * }' correctly include
// all their headers' symbols when Swift symbols are also present.

//--- UmbrellaSwift.swift
@_exported import UmbrellaFramework

// CHECK-DAG: "precise": "s:17UmbrellaFramework12SomeProtocolP"
public protocol SomeProtocol {}

//--- frameworks/UmbrellaFramework.framework/Modules/module.modulemap
framework module UmbrellaFramework [system] {
  umbrella header "UmbrellaFramework.h"
  export *
  module * { export * }
}

//--- frameworks/UmbrellaFramework.framework/Headers/UmbrellaFramework.h
#include "HeaderOne.h"
#include "HeaderTwo.h"
// CHECK-DAG: "precise": "c:@umbrellaVar"
static int umbrellaVar = 0;

//--- frameworks/UmbrellaFramework.framework/Headers/HeaderOne.h
// CHECK-DAG: "precise": "c:@varOne"
static int varOne = 1;

//--- frameworks/UmbrellaFramework.framework/Headers/HeaderTwo.h
// CHECK-DAG: "precise": "c:@varTwo"
static int varTwo = 2;

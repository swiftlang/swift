// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/swiftDeps)
// RUN: %empty-directory(%t/clangDeps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/client.swift -o %t/deps.json -I %t/swiftDeps -I %t/clangDeps
// RUN: %validate-json %t/deps.json | %FileCheck %s

// Ensure Swift module 'E' has a Swift overlay dependency on
// 'G', because Clang module 'G' is a visible dependency of Clang module 'E'
//
// CHECK-LABEL:        "modulePath": "{{.*}}E-{{.*}}.swiftmodule"
// CHECK:              "swiftOverlayDependencies": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "swift": "G"
// CHECK-NEXT:            }
// CHECK-NEXT:          ]

// Ensure Swift module 'A' does not have a Swift overlay dependency on
// 'G', because although 'A' depends on Clang module 'G', it does not export it
// and therefore it is not visible
//
// CHECK:        "modulePath": "{{.*}}A-{{.*}}.swiftmodule"
// CHECK-NOT:              "swiftOverlayDependencies": [

//--- swiftDeps/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -enable-library-evolution
@_exported import A
public func overlayFuncA() {}

//--- swiftDeps/E.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name E -enable-library-evolution
@_exported import E
public func overlayFuncE() {}

//--- swiftDeps/G.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name G -enable-library-evolution
@_exported import G
public func overlayFuncG() {}

//--- clangDeps/module.modulemap
module A {
  header "A.h"
  // No export *
}
module E {
  header "E.h"
  export *
}
module G {
  header "G.h"
  export *
}

//--- clangDeps/A.h
#include "G.h"
void funcA(void);

//--- clangDeps/E.h
#include "G.h"
void funcE(void);

//--- clangDeps/G.h
void funcG(void);

//--- client.swift
import A
import E

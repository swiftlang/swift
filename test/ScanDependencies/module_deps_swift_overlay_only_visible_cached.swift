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
// CHECK-DAG:               "swift": "G"
// CHECK-DAG:               "swift": "Y"
// CHECK:                 }

// Ensure Swift module 'G' has a Swift overlay dependency on
// 'Y', because Clang module 'Y' is a visible dependency of Clang module 'X'
//
// CHECK-LABEL:        "modulePath": "{{.*}}G-{{.*}}.swiftmodule"

//--- swiftDeps/E.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name E -enable-library-evolution
@_exported import E
public func overlayFuncE() {}

//--- swiftDeps/G.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name G -enable-library-evolution
@_exported import G
import X
public func overlayFuncG() {}

//--- swiftDeps/Y.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Y -enable-library-evolution
@_exported import Y
public func overlayFuncX() {}

//--- clangDeps/module.modulemap
module E {
  header "E.h"
  export *
}
module G {
  header "G.h"
  export *
}
module X {
  header "X.h"
  export *
}
module Y {
  header "Y.h"
  export *
}

//--- clangDeps/E.h
#include "G.h";
#include "X.h";
void funcE(void);

//--- clangDeps/G.h
void funcG(void);

//--- clangDeps/X.h
#include "Y.h";
void funcX(void);

//--- clangDeps/Y.h
void funcY(void);

//--- client.swift
import E

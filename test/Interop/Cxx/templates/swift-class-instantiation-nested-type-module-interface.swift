// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -emit-module -o %t/SwiftClassTemplateNestedTypeModule.swiftmodule %S/Inputs/SwiftClassTemplateNestedTypeModule.swift -I %S/Inputs -enable-library-evolution -swift-version 5
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftClassTemplateNestedTypeModule -I %t/ -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// REQUIRES: SR-13261

// CHECK: import ClassTemplateNestedTypeForSwiftModule
// CHECK: func receiveShipEngine(_ i: inout Space.__CxxTemplateInstN5Space4ShipIbEE)
// CHECK: func returnShipEngine() -> Space.__CxxTemplateInstN5Space4ShipIbEE

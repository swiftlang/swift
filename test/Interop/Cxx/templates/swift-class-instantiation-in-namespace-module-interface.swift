// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -emit-module -o %t/SwiftClassTemplateInNamespaceModule.swiftmodule %S/Inputs/SwiftClassInstantiationInNamespaceModule.swift -I %S/Inputs -enable-library-evolution -swift-version 5
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftClassTemplateInNamespaceModule -I %t/ -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: import ClassTemplateInNamespace
// CHECK: public func receiveShip(_ i: inout ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIbEE)
// CHECK: public func returnShip() -> ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIbEE
// CHECK: public func receiveShipWithEngine(_ i: inout ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIN6Engine8TurbojetEEE)
// CHECK: public func returnShipWithEngine() -> ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIN6Engine8TurbojetEEE

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %S/Inputs/SwiftClassInstantiationInNamespaceModule.swift -I %S/Inputs -enable-cxx-interop
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: import ClassTemplateInNamespace
// CHECK: public func receiveShip(_ i: inout ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIbEE)
// CHECK: public func returnShip() -> ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIbEE
// CHECK: public func receiveShipWithEngine(_ i: inout ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIN6Engine8TurbojetEEE)
// CHECK: public func returnShipWithEngine() -> ClassTemplateInNamespace.Space.__CxxTemplateInstN5Space4ShipIN6Engine8TurbojetEEE
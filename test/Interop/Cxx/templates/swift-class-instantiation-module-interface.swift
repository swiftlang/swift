// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %S/Inputs/SwiftClassInstantiationModule.swift -I %S/Inputs -enable-cxx-interop
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: import MagicWrapper

// CHECK: func makeWrappedMagicNumber() -> __ObjC.__CxxTemplateInst12MagicWrapperI10IntWrapperE

// CHECK: func readWrappedMagicNumber(_ i: inout __ObjC.__CxxTemplateInst12MagicWrapperI10IntWrapperE) -> Swift.CInt

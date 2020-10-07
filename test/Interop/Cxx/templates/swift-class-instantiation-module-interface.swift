// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SwiftClassInstantiationModule.swift -module-name SwiftClassInstantiationModule -emit-module -emit-module-path %t/ -I %S/Inputs -Xfrontend -enable-cxx-interop -parse-as-library
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftClassInstantiationModule -I %t -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: import MagicWrapper
// CHECK: import SwiftOnoneSupport

// CHECK: func makeWrappedMagicNumber() -> __CxxTemplateInst12MagicWrapperI10IntWrapperE

// CHECK: func readWrappedMagicNumber(_ i: inout __CxxTemplateInst12MagicWrapperI10IntWrapperE) -> CInt

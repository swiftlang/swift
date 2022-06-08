// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -emit-module -o %t/SwiftClassTemplateModule.swiftmodule %S/Inputs/SwiftClassInstantiationModule.swift -I %S/Inputs -enable-library-evolution -swift-version 5
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftClassTemplateModule -I %t/ -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s


// CHECK: import ClassTemplateForSwiftModule
// CHECK: func makeWrappedMagicNumber() -> __CxxTemplateInst12MagicWrapperI10IntWrapperE
// CHECK: func readWrappedMagicNumber(_ i: inout __CxxTemplateInst12MagicWrapperI10IntWrapperE) -> CInt
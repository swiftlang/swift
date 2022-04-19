// RUN: %target-swift-ide-test -print-module -module-to-print=ForwardDeclaredCxxRecord -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func usedInOtherHeader() -> ForwardDeclaredInOtherHeader!
// CHECK: class ForwardDeclaredInOtherHeader

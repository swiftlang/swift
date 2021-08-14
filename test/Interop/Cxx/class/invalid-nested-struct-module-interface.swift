// RUN: %target-swift-ide-test -print-module -module-to-print=InvalidNestedStruct -I %S/Inputs/ -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK-NOT: CannotImport
// CHECK-NOT: ForwardDeclaredSibling

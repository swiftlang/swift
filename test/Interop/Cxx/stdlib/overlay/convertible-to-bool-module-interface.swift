// RUN: %target-swift-ide-test -print-module -module-to-print=ConvertibleToBool -source-filename=x -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ConvertibleToBool -source-filename=x -I %S/Inputs -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ConvertibleToBool -source-filename=x -I %S/Inputs -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct BoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct NonConstBoolBox {
// CHECK: }

// CHECK: struct DualOverloadBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct ExplicitBoolBox : CxxConvertibleToBool {
// CHECK: }

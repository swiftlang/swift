// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithFunctionParameter -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithFunctionParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithFunctionParameter -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: @available(*, unavailable
// CHECK: struct function_wrapper<Fn> {
// CHECK: }

// CHECK: typealias FuncBoolToBool = function_wrapper<((CBool) -> CBool)>
// CHECK: typealias FuncVoidToBool = function_wrapper<(() -> CBool)>
// CHECK: typealias FuncBoolToVoid = function_wrapper<((CBool) -> Void)>
// CHECK: typealias FuncVoidToVoid = function_wrapper<(() -> Void)>
// CHECK: typealias FuncIntToInt = function_wrapper<((CInt) -> CInt)>
// CHECK: typealias FuncIntIntToInt = function_wrapper<((CInt, CInt) -> CInt)>
// CHECK: typealias FuncIntIntToVoid = function_wrapper<((CInt, CInt) -> Void)>
// CHECK: typealias FuncIntIntBoolToVoid = function_wrapper<((CInt, CInt, CBool) -> Void)>

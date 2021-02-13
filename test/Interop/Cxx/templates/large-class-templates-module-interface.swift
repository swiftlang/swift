// RUN: %target-swift-ide-test -print-module -module-to-print=LargeClassTemplates -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct HasTypeWithSelfAsParam<T> {
// CHECK: }

// CHECK: struct __CxxTemplateInst22HasTypeWithSelfAsParamIiE {
// CHECK:   typealias TT = __CxxTemplateInst22HasTypeWithSelfAsParamIS_IiEE
// CHECK:   init()
// CHECK: }

// CHECK: typealias WillBeInfinite = __CxxTemplateInst22HasTypeWithSelfAsParamIiE

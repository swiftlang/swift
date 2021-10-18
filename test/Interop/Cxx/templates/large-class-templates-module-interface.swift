// RUN: %target-swift-ide-test -print-module -module-to-print=LargeClassTemplates -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct HasTypeWithSelfAsParam<T> {
// CHECK: }

// CHECK: struct __CxxTemplateInst22HasTypeWithSelfAsParamIiE {
// CHECK:   typealias TT = __CxxTemplateInst22HasTypeWithSelfAsParamIS_IiEE
// CHECK:   init()
// CHECK: }

// CHECK: typealias WillBeInfinite = __CxxTemplateInst22HasTypeWithSelfAsParamIiE

// Make sure we fail to import super slow templates.
// CHECK-NOT: __CxxTemplateInstN14RegressionTest7ValExprINS_9SliceExprINS_5ArrayIiEELi1EEEEE
// TODO: we should not be importing functions that use this type in their
// signature (such as the function below).
// CHECK: mutating func test1() -> RegressionTest.__CxxTemplateInstN14RegressionTest7ValExprINS_9SliceExprINS_5ArrayIiEELi1EEEEE

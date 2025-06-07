// RUN: %target-swift-ide-test -print-module -module-to-print=LargeClassTemplates -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasTypeWithSelfAsParam<T> {
// CHECK: }

// CHECK: struct HasTypeWithSelfAsParam<CInt> {
// CHECK:   init()
// CHECK:   typealias TT = HasTypeWithSelfAsParam<HasTypeWithSelfAsParam<CInt>>
// CHECK: }

// CHECK: typealias WillBeInfinite = HasTypeWithSelfAsParam<CInt>

// TODO: we should not be importing functions that use this type in their
// signature (such as the function below).
// CHECK: mutating func test1() -> RegressionTest.ValExpr<RegressionTest.SliceExpr<RegressionTest.SliceExpr<RegressionTest.Array<CInt>, _CInt_1>, _CInt_1>>

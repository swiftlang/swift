// RUN: %target-swift-ide-test -print-module -module-to-print=TemplateTypeParameterNotInSignature -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func templateTypeParamNotUsedInSignature<T>(T: T.Type)
// CHECK: func multiTemplateTypeParamNotUsedInSignature<T, U>(T: T.Type, U: U.Type)
// CHECK: func multiTemplateTypeParamOneUsedInSignature<T, U>(_ u: U, T: T.Type) -> U
// CHECK: func multiTemplateTypeParamNotUsedInSignatureWithUnrelatedParams<T, U>(_ x: Int32, _ y: Int32, T: T.Type, U: U.Type)
// CHECK: func templateTypeParamUsedInReturnType<T>(_ x: Int32) -> T
// CHECK: func templateTypeParamUsedInReferenceParam<T>(_ t: inout T) -> T
// CHECK: @available(*, unavailable, message: "Variadic function is unavailable")
// CHECK: func templateTypeParamNotUsedInSignatureWithVarargs<T, U>(T: T.Type, U: U.Type, _ varargs: Any...)
// CHECK: @available(*, unavailable, message: "Variadic function is unavailable")
// CHECK: func templateTypeParamNotUsedInSignatureWithVarargsAndUnrelatedParam<T, U, V>(_ x: Int32, T: T.Type, U: U.Type, V: V.Type, _ varargs: Any...)

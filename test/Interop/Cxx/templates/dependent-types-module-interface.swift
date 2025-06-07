// Waiting for support for dependent types to be added back: rdar://103530256&90587703&89090706&89090631&89034704&89034440&83406001&83367285
// XFAIL: *

// RUN: %target-swift-ide-test -print-module -module-to-print=DependentTypes -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func differentDependentArgAndRet<T, U>(_ a: Any, T: T.Type, U: U.Type) -> Any
// CHECK: func dependantReturnTypeInferred<T>(_ a: T) -> Any
// CHECK: func dependantReturnTypeSameAsArg<T>(_ a: Any, T: T.Type) -> Any
// CHECK: func complexDependantReturnTypeInferred<T>(_ a: T) -> Any
// CHECK: func multipleArgs<T>(_ a: Any, _ b: T, _ c: Int32) -> Any
// CHECK: func multipleDependentArgsInferred<T, U>(_ a: Any, _ b: Any, _ c: T, _ d: U) -> Any
// CHECK: func multipleDependentArgs<T, U>(_ a: Any, _ b: Any, T: T.Type, U: U.Type) -> Any
// CHECK: func refToDependent<T>(_ a: inout T) -> Any
// TODO: Currently not imported (rdar://89034440).
// CHECK-NOT: dependentRef
// CHECK-NOT: dependentRefAndRefInferred

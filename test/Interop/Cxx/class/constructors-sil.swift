// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-sil %s | %FileCheck %s

import Constructors

// The most important thing to test here is that the constructor result is returned
// with an @out attribute.
// CHECK: [[VAR:%[0-9]+]] = alloc_stack $ConstructorWithParam
// CHECK: [[TYPE:%[0-9]+]] = metatype $@thin ConstructorWithParam.Type
// CHECK: [[LITERAL:%[0-9]+]] = integer_literal $Builtin.Int32, 42
// CHECK: [[INT:%[0-9]+]] = struct $Int32 ([[LITERAL]] : $Builtin.Int32)
// CHECK: [[FUNC:%[0-9]+]] = function_ref @_ZN20ConstructorWithParamC1Ei : $@convention(c) (Int32, @thin ConstructorWithParam.Type) -> @out ConstructorWithParam
// CHECK: %{{[0-9]+}} = apply [[FUNC]]([[VAR]], [[INT]], [[TYPE]]) : $@convention(c) (Int32, @thin ConstructorWithParam.Type) -> @out ConstructorWithParam
let _ = ConstructorWithParam(42)

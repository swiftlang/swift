// RUN: %target-swift-frontend -I %S/Inputs -enable-cxx-interop -emit-ir %s | %FileCheck %s

import CxxConstructors

// Note:
// - The `this` parameter should carry a `noalias` attribute, as it is
//   guaranteed that nothing will alias the object before it has been fully
//   constructed. It should also carry an `sret` attribute to indicate that this
//   is an out parameter for a structure being returned by the function.
// - The `this` parameter should _not_ carry a `nocapture` attribute (unlike
//   Swift constructors that return their result indirectly) because the C++ has
//   explicit access to `this` and may capture it.
// CHECK: call void @_ZN20ConstructorWithParamC2Ei(%struct.ConstructorWithParam* noalias sret %{{[0-9]+}}, i32 42)
let _ = ConstructorWithParam(42)

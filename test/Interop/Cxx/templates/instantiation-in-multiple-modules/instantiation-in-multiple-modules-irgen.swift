// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import A
import B
// Both A and B reference WrapperInt, and clang will instantiate two different redecls for WrapperInt.

let w = WrapperInt()
// The constructor of Wrapper calls Wrapper::foo, we need to make sure that Wrapper::foo is emitted,
// otherwise a linker error occurs.
// CHECK: define linkonce_odr{{( dso_local)?}} void @{{_ZN7WrapperIiE3fooEv|"\?foo@\?\$Wrapper@H@@QEAAXXZ"}}

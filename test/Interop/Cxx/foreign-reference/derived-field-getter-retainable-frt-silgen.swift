// RUN: %target-swift-emit-sil -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import DerivedFieldGetterReturnsOwnedFRT

func testGetX() -> CInt {
    let derived = DerivedFieldFRT()
    return derived.value.testVal
}

let _ = testGetX()

// CHECK:  function_ref @{{.*}}__synthesizedBaseGetterAccessor_{{.*}} : $@convention(cxx_method) (@in_guaranteed DerivedFieldFRT) -> @owned RefCounted

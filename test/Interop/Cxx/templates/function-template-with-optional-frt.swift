// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking | %FileCheck %s

import FunctionTemplateWithOptionalFrt

// CHECK-LABEL: define {{.*}}@{{.*}}testPassThrough
func testPassThrough(x: FRTBase) -> FRTBase {
  return passThrough(x)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testPassThroughOptional
func testPassThroughOptional(x: FRTBase?) -> FRTBase? {
  return passThrough(x)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testCast
func testCast(x: FRTBase) -> FRTDerived? {
  return cast(x)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testDynamicCast
func testDynamicCast(x: FRTBase) -> FRTDerived? {
  return dynamicCast(x)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testDowncast
func testDowncast(x: FRTBase) -> FRTDerived? {
  return downcast(x)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testPassThroughLet
func testPassThroughLet(base: FRTBase) {
  let _: FRTBase = passThrough(base)
  let _: FRTBase? = passThrough(base)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testCastLet
func testCastLet(base: FRTBase) {
  let _: FRTDerived = cast(base)
  let _: FRTDerived? = cast(base)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testDynamicCastLet
func testDynamicCastLet(base: FRTBase) {
  let _: FRTDerived = dynamicCast(base)
  let _: FRTDerived? = dynamicCast(base)
}

// CHECK-LABEL: define {{.*}}@{{.*}}testDowncastLet
func testDowncastLet(base: FRTBase) {
  let _: FRTDerived = downcast(base)
  let _: FRTDerived? = downcast(base)
}

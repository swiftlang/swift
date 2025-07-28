
// Check that we get the expected errors for incorrect uses of noncopyable
// imported types with both C and C++ interoperability.

// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -verify -DERRORS
// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -verify -DERRORS -cxx-interoperability-mode=default

// Check that we get the expected SIL
// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -o - | %FileCheck -check-prefix CHECK-SIL %s
// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -o - -cxx-interoperability-mode=default| %FileCheck  -check-prefix CHECK-SIL %s

// Check that we get the expected IR

// RUN: %target-swift-frontend -emit-ir -I %S/Inputs/ %s -o - | %FileCheck -check-prefix CHECK-IR %s
// RUN: %target-swift-frontend -emit-ir -I %S/Inputs/ %s -o - -cxx-interoperability-mode=default | %FileCheck -check-prefix CHECK-IR %s

import NoncopyableStructs

// CHECK-IR-LABEL: define hidden swiftcc void @"$s19noncopyable_structs9consumeNCyySo11NonCopyableVnF"(float %0, float %1) #0 {
// CHECK-IR: call ptr @"$sSo11NonCopyableVWOh"
// CHECK-IR: define linkonce_odr hidden ptr @"$sSo11NonCopyableVWOh"(ptr %0)
// CHECK-IR-NEXT: entry:
// CHECK-IR-NEXT: ret ptr
func consumeNC(_ nc: consuming NonCopyable) { }

func testNC() {
  let nc = NonCopyable() // expected-error{{'nc' consumed more than once}}
  consumeNC(nc) // expected-note{{consumed here}}

  #if ERRORS
  consumeNC(nc) // expected-note{{consumed again here}}
  #endif
}

func consumeNCWithDeinit(_ nc: consuming NonCopyableWithDeinit) { }

func testNCWithDeinit() {
  let nc = NonCopyableWithDeinit() // expected-error{{'nc' consumed more than once}}
  consumeNCWithDeinit(nc) // expected-note{{consumed here}}

  #if ERRORS
  consumeNCWithDeinit(nc) // expected-note{{consumed again here}}
  #endif
}

// CHECK-SIL: sil shared @$sSo21NonCopyableWithDeinitVfD : $@convention(method) (@owned NonCopyableWithDeinit) -> () {
// CHECK-SIL: bb0([[SELF:%[0-9]+]] : $NonCopyableWithDeinit):
// CHECK-SIL: [[SELF_ALLOC:%[0-9]+]] = alloc_stack $NonCopyableWithDeinit
// CHECK-SIL:  store [[SELF]] to [[SELF_ALLOC]]
// CHECK-SIL: [[SELF_RELOAD:%[0-9]+]] = load [[SELF_ALLOC]]
// CHECK-SIL: [[FN:%[0-9]+]] = function_ref @{{.*}}freeNonCopyableWithDeinit : $@convention(c) (NonCopyableWithDeinit) -> ()
// CHECK-SIL:  apply [[FN]]([[SELF_RELOAD]]) : $@convention(c) (NonCopyableWithDeinit) -> ()

// CHECK-IR-LABEL: define hidden swiftcc void @"$s19noncopyable_structs19consumeNCWithDeinityySo015NonCopyableWithE0VnF"
// CHECK-IR: call swiftcc void @"$sSo21NonCopyableWithDeinitVfD"

// CHECK-IR-LABEL: define {{.*}} swiftcc void @"$sSo21NonCopyableWithDeinitVfD"
// CHECK-IR: {{(call|invoke)}} void @{{.*}}freeNonCopyableWithDeinit

// CHECK-SIL-LABEL: sil_moveonlydeinit NonCopyableWithDeinit {
// CHECK-SIL: @$sSo21NonCopyableWithDeinitVfD

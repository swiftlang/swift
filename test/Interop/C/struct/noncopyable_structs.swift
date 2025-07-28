
// Check that we get the expected errors for incorrect uses of noncopyable
// imported types with both C and C++ interoperability.

// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -verify -DERRORS
// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ %s -verify -DERRORS -cxx-interoperability-mode=default

// Check that we get the expected IR

// RUN: %target-swift-frontend -emit-ir -I %S/Inputs/ %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %S/Inputs/ %s -o - -cxx-interoperability-mode=default| %FileCheck %s

import NoncopyableStructs

// CHECK-LABEL: define hidden swiftcc void @"$s19noncopyable_structs9consumeNCyySo11NonCopyableVnF"(float %0, float %1) #0 {
// CHECK: call ptr @"$sSo11NonCopyableVWOh"
// CHECK: define linkonce_odr hidden ptr @"$sSo11NonCopyableVWOh"(ptr %0)
// CHECK-NEXT: entry:
// CHECK-NEXT: ret ptr
func consumeNC(_ nc: consuming NonCopyable) { }

func testNC() {
  let nc = NonCopyable() // expected-error{{'nc' consumed more than once}}
  consumeNC(nc) // expected-note{{consumed here}}

  #if ERRORS
  consumeNC(nc) // expected-note{{consumed again here}}
  #endif
}

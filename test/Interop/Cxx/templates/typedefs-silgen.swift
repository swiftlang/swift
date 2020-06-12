// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import Typedefs

public func foo() -> CInt {
  let banana = Banana()
  var peeled: PeeledBanana = PeeledBanana(fruit: banana)
  return peeled.peeledTaste()
}

// CHECK: sil @$s4main3foos5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK: %0 = struct $Banana ()
// CHECK: %2 = alloc_stack $_ZTS4PeelI6BananaE, var, name "peeled"
// CHECK: %3 = struct $_ZTS4PeelI6BananaE (%0 : $Banana)
// CHECK: store %3 to %2 : $*_ZTS4PeelI6BananaE
// CHECK: %6 = function_ref @_ZNK4PeelI6BananaE11peeledTasteEv : $@convention(c) (@inout _ZTS4PeelI6BananaE) -> Int32
// CHECK: %7 = apply %6(%5) : $@convention(c) (@inout _ZTS4PeelI6BananaE) -> Int32

// CHECK: // clang name: Peel<Banana>::peeledTaste
// CHECK: sil [clang _ZTS4PeelI6BananaE.peeledTaste] @_ZNK4PeelI6BananaE11peeledTasteEv : $@convention(c) (@inout _ZTS4PeelI6BananaE) -> Int32

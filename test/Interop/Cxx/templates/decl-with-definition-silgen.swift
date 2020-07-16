// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RU N: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// RU N: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import DeclWithDefinition

public func foo() -> CInt {
  let a = Arg()
  var t = HasDefinition(t: a)
  return t.callMethod()
}

// CHECK: sil @$s4main3foos5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK: %3 = struct $_ZTS3TplI3ArgE (%0 : $Arg)
// CHECK: function_ref _ZNK3TplI3ArgE10callMethodEv
// CHECK: %6 = function_ref @_ZNK3TplI3ArgE10callMethodEv : $@convention(c) (@inout _ZTS3TplI3ArgE) -> Int32

// CHECK: _ZNK3TplI3ArgE10callMethodEv
// CHECK: clang name: Tpl<Arg>::callMethod
// CHECK: sil [clang _ZTS3TplI3ArgE.callMethod] @_ZNK3TplI3ArgE10callMethodEv : $@convention(c) (@inout _ZTS3TplI3ArgE) -> Int32

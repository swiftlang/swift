// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import PartiallyPreDefinedClassTemplate

public func getWrappedMagicInt() -> CInt {
  let myInt = IntWrapper(value: 21)
  let magicInt = PartiallyPreDefinedMagicallyWrappedInt(t: myInt)
  return magicInt.getValuePlusArg(32)
}

// CHECK: // getWrappedMagicInt()
// CHECK: sil @$s4main18getWrappedMagicInts5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK: [[INT_WRAPPER:%.*]] = struct $IntWrapper ([[_:%.*]] : $Int32)
// CHECK: [[_:%.*]] = struct $MagicWrapper<IntWrapper> ([[INT_WRAPPER]] : $IntWrapper)
// CHECK: [[_:%.*]] = function_ref @$sSo0030MagicWrapperIntWrapper_bHAFhxbV15getValuePlusArgys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed MagicWrapper<IntWrapper>) -> Int32

// CHECK: sil {{.*}}[clang MagicWrapper<IntWrapper>.getValuePlusArg] @$sSo0030MagicWrapperIntWrapper_bHAFhxbV15getValuePlusArgys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed MagicWrapper<IntWrapper>) -> Int32

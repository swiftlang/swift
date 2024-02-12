// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import Mangling

public func recvInstantiation(_ i: inout WrappedMagicInt) {}

// CHECK: // recvInstantiation(_:)
// CHECK: sil @$s4main17recvInstantiationyySo0024MagicWrapperCInt_npAIefbVzF : $@convention(thin) (@inout MagicWrapper<CInt>) -> ()

public func recvInstantiation(_ i: inout WrappedMagicBool) {}

// CHECK: // recvInstantiation(_:)
// CHECK: sil @$s4main17recvInstantiationyySo0025MagicWrapperCBool_lsFCfibVzF : $@convention(thin) (@inout MagicWrapper<CBool>) -> ()

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: // returnInstantiation()
// CHECK: sil @$s4main19returnInstantiationSo0024MagicWrapperCInt_npAIefbVyF : $@convention(thin) () -> MagicWrapper<CInt>

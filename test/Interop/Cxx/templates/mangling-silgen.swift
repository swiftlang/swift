// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import Mangling

public func recvInstantiation(_ i: inout WrappedMagicInt) {}

// CHECK: // recvInstantiation(_:)
// CHECK: sil @$s4main17recvInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF : $@convention(thin) (@inout MagicWrapper<Int32>) -> ()

public func recvInstantiation(_ i: inout WrappedMagicBool) {}

// CHECK: // recvInstantiation(_:)
// CHECK: sil @$s4main17recvInstantiationyySo34__CxxTemplateInst12MagicWrapperIbEVzF : $@convention(thin) (@inout MagicWrapper<Bool>) -> ()

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: // returnInstantiation()
// CHECK: sil @$s4main19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF : $@convention(thin) () -> MagicWrapper<Int32>

// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import Mangling

public func receiveInstantiation(_ i: inout WrappedMagicBool) {}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main20receiveInstantiationyySo0025MagicWrapperCBool_lsFCfibVzF"(ptr {{(nocapture|captures\(none\))}} dereferenceable(1) %0)

public func receiveInstantiation(_ i: inout WrappedMagicInt) {}

// Don't forget to update manglings.txt when changing s4main20receiveInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF
// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main20receiveInstantiationyySo0024MagicWrapperCInt_npAIefbVzF"(ptr {{(nocapture|captures\(none\))}} dereferenceable(1) %0)

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// Don't forget to update manglings.txt when changing s4main19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF
// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main19returnInstantiationSo0024MagicWrapperCInt_npAIefbVyF"()


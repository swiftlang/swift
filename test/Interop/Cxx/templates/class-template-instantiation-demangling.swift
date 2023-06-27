// RUN: %target-swift-emit-ir %use_no_opaque_pointers %s -I %S/Inputs -enable-experimental-cxx-interop | grep 'define.*swiftcc.*$' | xargs %swift-demangle | %FileCheck %s
// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | grep 'define.*swiftcc.*$' 

import Mangling

public func receiveInstantiation(_ i: inout WrappedMagicInt) {}

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: $s4main20receiveInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF(%TSo34__CxxTemplateInst12MagicWrapperIiEV* ---> @$s4main20receiveInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF(%TSo34__CxxTemplateInst12MagicWrapperIiEV* 
// CHECK: $s4main19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF() ---> @$s4main19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF() 

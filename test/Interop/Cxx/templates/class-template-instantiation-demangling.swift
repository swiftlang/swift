// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | grep 'define.*swiftcc.*$' | grep -o '[[:alnum:]]*__CxxTemplateInst[[:alnum:]]*' | xargs %swift-demangle | %FileCheck %s

import Mangling

public func receiveInstantiation(_ i: inout WrappedMagicInt) {}

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: $s4main20receiveInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF ---> main.receiveInstantiation(inout __C.__CxxTemplateInst12MagicWrapperIiE) -> ()
// CHECK: $s4main19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF ---> main.returnInstantiation() -> __C.__CxxTemplateInst12MagicWrapperIiE

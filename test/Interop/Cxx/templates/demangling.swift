// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | grep 'define.*swiftcc.*$' | grep -o '[[:alnum:]]*__CxxTemplateInst[[:alnum:]]*' | xargs %swift-demangle | %FileCheck %s

import Mangling

public func receiveInstantiation(_ i: inout WrappedMagicInt) {}

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: $s10demangling20receiveInstantiationyySo34__CxxTemplateInst12MagicWrapperIiEVzF ---> demangling.receiveInstantiation(inout __C.__CxxTemplateInst12MagicWrapperIiE) -> ()
// CHECK: $s10demangling19returnInstantiationSo34__CxxTemplateInst12MagicWrapperIiEVyF ---> demangling.returnInstantiation() -> __C.__CxxTemplateInst12MagicWrapperIiE

// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | grep 'define.*swiftcc.*$' | xargs %swift-demangle | %FileCheck %s

import Mangling

public func receiveInstantiation(_ i: inout WrappedMagicInt) {}

public func returnInstantiation() -> WrappedMagicInt {
  return WrappedMagicInt()
}

// CHECK: $s4main20receiveInstantiationyySo0024MagicWrapperCInt_npAIefbVzF(ptr ---> @$s4main20receiveInstantiationyySo0024MagicWrapperCInt_npAIefbVzF(ptr
// CHECK: $s4main19returnInstantiationSo0024MagicWrapperCInt_npAIefbVyF() ---> @$s4main19returnInstantiationSo0024MagicWrapperCInt_npAIefbVyF()

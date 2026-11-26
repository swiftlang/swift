// RUN: not %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

// Member functions of class template specializations are instantiated when
// IRGen emits them, so errors in their bodies are reported then.

import ClassTemplateInstantiationErrors

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::CannotBeInstantianted' requested here

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::memberWrongType' requested here

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::argWrongType' requested here
public func clangErrorReportedOnInstantiation() {
  _ = CannotBeInstantiantedIntWrapper(CChar(0), IntWrapper())
  var z = CannotBeInstantiantedIntWrapper(IntWrapper())
  z.memberWrongType()
  z.argWrongType(IntWrapper())
}

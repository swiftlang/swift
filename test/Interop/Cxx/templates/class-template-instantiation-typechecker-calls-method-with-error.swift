// RUN: not %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

import ClassTemplateInstantiationErrors

// CHECK: error: no member named 'doesNotExist' in 'IntWrapper'
// CHECK: note: in instantiation of member function 'CannotBeInstantianted<IntWrapper>::memberWrongType' requested here
public func test() {
  var y = CannotBeInstantianted<IntWrapper>(IntWrapper())
  y.callsMethodWithError()
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name SuperLibrary %t/library.swift -emit-module-path %t/SuperLibrary.swiftmodule -I %S/Inputs -I %t -cxx-interoperability-mode=default -disable-availability-checking
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -I %S/Inputs -cxx-interoperability-mode=default -o %t/client.ll -disable-availability-checking

//--- module.modulemap
module InheritedMethods {
  header "inherited.h"
  requires cplusplus
}

//--- inherited.h
struct MethodBase {
  int value = 0;
  int add(int amount) { return value += amount; }
  int read() const { return value; }
};
struct MethodDerived : MethodBase {};
struct MethodTwiceDerived : MethodDerived {};

//--- library.swift
import SuperVirtualDispatch
import InheritedMethods

extension Derived {
  @inlinable
  public func callSuper() -> Int32 {
    super.virtualMethod()
  }
  @inlinable
  public func captureSuper() -> () -> Int32 {
    super.virtualMethod
  }
}

@inlinable
public func add(_ value: inout MethodTwiceDerived) -> CInt {
  value.add(1)
}
@inlinable
public func capturedRead(_ value: MethodDerived) -> () -> CInt {
  value.read
}

//--- client.swift
import SuperVirtualDispatch
import SuperLibrary
import InheritedMethods

public func callSuperInLibrary(_ value: Derived) -> Int32 {
  value.callSuper() + value.captureSuper()()
}
public func inheritedInLibrary(_ value: inout MethodTwiceDerived) -> CInt {
  add(&value)
}
public func capturedInheritedInLibrary(_ value: MethodDerived) -> CInt {
  capturedRead(value)()
}

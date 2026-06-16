// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -disable-availability-checking

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import SuperVirtualDispatch

extension ConcreteDerived {
  public func callSuperPure() -> Int32 {
    return super.pureMethod() // expected-error {{cannot use 'super' to call C++ pure virtual method 'pureMethod()'; it has no base class implementation}}
  }

  public func callPureImpl() -> Int32 {
    return pureMethod()
  }

  public func storeSuperPure() {
    let _ = super.pureMethod // expected-error {{cannot use 'super' to call C++ pure virtual method 'pureMethod()'; it has no base class implementation}}
  }
}

// A virtual base is not imported as a Swift superclass, so 'super' is unavailable.
extension VirtuallyDerived {
  public func callSuperVirtual() -> Int32 {
    return super.virtualMethod() // expected-error {{'super' cannot be used in extension of class 'VirtuallyDerived' because it has no superclass}}
  }
}

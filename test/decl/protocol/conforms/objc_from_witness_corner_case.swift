// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -print-ast %s | %FileCheck %s

// REQUIRES: objc_interop

// This bug required an elaborate setup where isObjC() was checked prior
// to validateDecl() getting called on a declaration. In this case, we
// did not infer @objc from witnessed protocol requirements as required.
//
// https://bugs.swift.org/browse/SR-10257

@objc public protocol P {
  @objc optional func f()
}

public class Other {
  // This triggers a walk over all nominals in the file, collecting
  // @objc members into the dynamic dispatch lookup table.
  let a = (Base() as AnyObject).g()
}

@objc public class Base : P {
  @objc public func g() -> Int { return 0 }
}

public class D : Base {
  // This method witnesses P.f() and so it should be @objc.
  //
  // CHECK-LABEL: @objc public func f()
  public func f() {}
}


/// Recover under diagnoseUnintendedObjCMethodOverrides.
/// rdar://138764733

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: objc_interop

// RUN: %target-swift-frontend -emit-module %t/HiddenDep.swift -I %t \
// RUN:   -o %t/HiddenDep.swiftmodule \
// RUN:   -disable-objc-attr-requires-foundation-module

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -o %t/Lib.swiftmodule \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -disable-objc-attr-requires-foundation-module

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -disable-objc-attr-requires-foundation-module

//--- HiddenDep.swift
@objc
public class HiddenType {}

//--- Lib.swift
internal import HiddenDep

@objc public class Redecl {
  @objc
  func methodWithXref() -> HiddenType { fatalError() }
}

//--- Client.swift
import Lib

extension Redecl {
  @objc(methodWithXref)
  func methodWithXref_alias() { }
}

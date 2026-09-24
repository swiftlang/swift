//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-constructors.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ConstructorLibrary %t/library.swift -emit-module-path %t/ConstructorLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.ll
// RUN: %target-swift-frontend -emit-module -module-name ConstructorLibrary %t/library.swift -emit-module-path %t/ConstructorLibrary.swiftmodule -emit-module-interface-path %t/ConstructorLibrary.swiftinterface -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -compile-module-from-interface %t/ConstructorLibrary.swiftinterface -o %t/ConstructorLibrary.swiftmodule -I %t -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/interface-client.ll

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- library.swift
import AnnotatedConstructors
@inlinable
public func construct(_ value: CInt) throws -> Checked {
  try Checked(value)
}
@inlinable
public func constructorReference() -> (CInt) throws -> Checked {
  Checked.init
}
@inlinable
public func overloadedConstructorReference() -> (CDouble) throws -> Checked {
  Checked.init
}
@inlinable
public func inheritedConstructorReference() -> (CInt) throws -> Inherited {
  Inherited.init
}
@inlinable
public func emptyNontrivialConstructorReference() -> (CInt) throws -> EmptyNontrivial {
  EmptyNontrivial.init
}
@inlinable
public func explicitDerivedConstructorReference() -> (CInt) throws -> ExplicitDerived {
  ExplicitDerived.init
}
@inlinable
public func privateConstructorReference() -> (CInt) throws -> PrivateValue {
  PrivateValue.init
}
@inlinable
public func constructMoveOnly(_ value: CInt) throws -> MoveOnly {
  try MoveOnly(value)
}
@inlinable
public func moveOnlyConstructorReference() -> (CInt) throws -> MoveOnly {
  MoveOnly.init
}

//--- client.swift
import AnnotatedConstructors
import ConstructorLibrary
public func useConstructors(_ value: CInt) throws -> CInt {
  let direct = try construct(value)
  let captured = try constructorReference()(value)
  let overloaded = try overloadedConstructorReference()(CDouble(value))
  let inherited = try inheritedConstructorReference()(value)
  _ = try emptyNontrivialConstructorReference()(value)
  _ = try explicitDerivedConstructorReference()(value)
  _ = try privateConstructorReference()(value)
  let moveOnly = try constructMoveOnly(value)
  let capturedMoveOnly = try moveOnlyConstructorReference()(value)
  return direct.value + captured.value + overloaded.value + inherited.value + moveOnly.value + capturedMoveOnly.value
}

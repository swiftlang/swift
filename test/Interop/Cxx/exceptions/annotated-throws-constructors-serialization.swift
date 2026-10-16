// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-constructors.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ConstructorLibrary %t/library.swift -emit-module-path %t/ConstructorLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil
// RUN: %FileCheck %s < %t/client.sil
// RUN: %target-swift-frontend -emit-module -module-name ConstructorLibrary %t/library.swift -emit-module-path %t/ConstructorLibrary.swiftmodule -emit-module-interface-path %t/ConstructorLibrary.swiftinterface -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -compile-module-from-interface %t/ConstructorLibrary.swiftinterface -o %t/ConstructorLibrary.swiftmodule -I %t -cxx-interoperability-mode=default
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/interface-client.sil
// RUN: %FileCheck %s < %t/interface-client.sil

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- library.swift
import AnnotatedThrowingConstructors
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
import AnnotatedThrowingConstructors
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

// The client reconstructs the adapter of each initializer that the inlined
// library code references. The hex digits spell the Itanium mangled name of
// the C++ constructor, such as _ZN7CheckedC1Ei.
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E37436865636B656443314569{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E37436865636B656443314564{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E39496E6865726974656443493137436865636B65644569{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E3135456D7074794E6F6E7472697669616C43314569{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E31354578706C696369744465726976656443314569{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E31325072697661746556616C756543314569{{.*}} : $@convention(c)
// CHECK-DAG: function_ref @$sSo{{[0-9]+}}__swift_cxx_exception_5F5A4E384D6F76654F6E6C7943314569{{.*}} : $@convention(c)

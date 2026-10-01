// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-methods.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ThrowingMethodsLibrary %t/library.swift -emit-module-path %t/ThrowingMethodsLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-availability-checking
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil -disable-availability-checking
// RUN: %FileCheck %s --check-prefix=SIL < %t/client.sil
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.ll -disable-availability-checking
// RUN: %FileCheck %s --check-prefix=IR < %t/client.ll

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- library.swift
import AnnotatedThrowingMethods

@inlinable
public func read(_ value: Counter, fail: Bool) throws -> CInt {
  try value.read(fail)
}
@inlinable
public func capturedRead(_ value: Counter) -> (Bool) throws -> CInt {
  value.read
}
@inlinable
public func add(_ value: inout Counter, fail: Bool) throws -> CInt {
  try value.add(1, fail)
}
@inlinable
public func inheritedAdd(_ value: inout TwiceDerivedCounter, fail: Bool) throws -> CInt {
  try value.add(2, fail)
}
extension ReferenceDerived {
  @inlinable
  public func baseChecked(_ fail: Bool) throws -> CInt {
    try super.checked(fail)
  }
}

//--- client.swift
import AnnotatedThrowingMethods
import ThrowingMethodsLibrary

public func useLibrary(_ value: inout Counter, fail: Bool) throws -> CInt {
  let captured = capturedRead(value)
  let original = try read(value, fail: fail)
  return try captured(fail) + add(&value, fail: fail) + original
}
public func useReference(_ value: ReferenceDerived, fail: Bool) throws -> CInt {
  try value.checked(fail) + value.baseChecked(fail)
}
public func useInherited(_ value: inout TwiceDerivedCounter, fail: Bool) throws -> CInt {
  try inheritedAdd(&value, fail: fail)
}

// The client reconstructs the adapters referenced by the deserialized library
// code, including those behind a `super` call and an inherited method.
// SIL-DAG: sil {{.*}}[clang Counter.__swift_cxx_exception_5F5A4E5237436F756E74657233616464456962] {{.*}} : $@convention(cxx_method)
// SIL-DAG: sil {{.*}}[clang Counter.__swift_cxx_exception_5F5A4E4B37436F756E74657234726561644562] {{.*}} : $@convention(cxx_method)
// SIL-DAG: sil {{.*}}[clang ReferenceDerived.__swift_cxx_exception_{{.*}}] {{.*}} : $@convention(cxx_method)
// SIL-DAG: sil {{.*}}[clang ReferenceBase.__swift_cxx_exception_5F5A4E4B31335265666572656E63654261736537636865636B65644562] {{.*}} : $@convention(cxx_method)
// SIL-DAG: sil {{.*}}[clang TwiceDerivedCounter.__swift_cxx_exception_{{.*}}] {{.*}} : $@convention(cxx_method)

// IR-DAG: invoke {{.*}} @_ZNR7Counter3addEib(
// IR-DAG: invoke {{.*}} @_ZNK7Counter4readEb(

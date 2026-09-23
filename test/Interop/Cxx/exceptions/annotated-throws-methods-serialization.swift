// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-methods.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ThrowingMethodsLibrary %t/library.swift -emit-module-path %t/ThrowingMethodsLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-availability-checking
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil -disable-availability-checking
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.ll -disable-availability-checking

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

// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ThrowingLibrary %t/library.swift -emit-module-path %t/ThrowingLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.ll

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- library.swift
import AnnotatedThrows

@inlinable
public func divide(_ numerator: CInt, _ denominator: CInt) throws -> CInt {
  try checkedDivide(numerator, denominator)
}

@inlinable
public func divisionFunction() -> (CInt, CInt) throws -> CInt {
  checkedDivide
}

@inlinable
public func staticFunction() -> (CInt) throws -> CInt {
  StaticFunctions.checked
}

@inlinable
public func namespaceFunction() -> (Double) throws -> Double {
  Numbers.checked
}

//--- client.swift
import AnnotatedThrows
import ThrowingLibrary

public func useLibrary(_ value: CInt) throws -> CInt {
  let captured = divisionFunction()
  let staticMethod = staticFunction()
  _ = try namespaceFunction()(1.5)
  return try divide(value, 2) + captured(value, 3) + staticMethod(value)
    + checkedDivide(value, 4)
}

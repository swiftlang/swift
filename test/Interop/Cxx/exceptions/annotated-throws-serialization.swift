// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -enable-default-cmo -module-name ThrowingLibrary %t/library.swift -emit-module-path %t/ThrowingLibrary.swiftmodule -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging
// RUN: %target-swift-frontend -emit-sil -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.sil
// RUN: %FileCheck %s --check-prefix=SIL < %t/client.sil
// RUN: %target-swift-frontend -emit-ir -O -enable-default-cmo %t/client.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -o %t/client.ll
// RUN: %FileCheck %s --check-prefix=IR < %t/client.ll

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

// Same-scope overloads are told apart by the type of the facade they are
// anchored on.
@inlinable
public func overloads(_ value: CInt) throws -> Double {
  Double(try overloaded(value)) + (try overloaded(1.5))
}

@inlinable
public func redeclared(_ value: CInt) throws -> CInt {
  try annotatedFirst(value)
}

//--- client.swift
import AnnotatedThrows
import ThrowingLibrary

public func useLibrary(_ value: CInt) throws -> CInt {
  let captured = divisionFunction()
  let staticMethod = staticFunction()
  _ = try namespaceFunction()(1.5)
  _ = try overloads(value) + Double(redeclared(value))
  return try divide(value, 2) + captured(value, 3) + staticMethod(value)
    + checkedDivide(value, 4)
}

// The client reconstructs the adapters referenced by the deserialized library
// code. Adapter names hex-encode the mangled name of the C++ function, e.g.
// 5F5A3133636865636B65644469766964656969 is _Z13checkedDivideii.
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A3133636865636B65644469766964656969] {{.*}} : $@convention(c) (Int32, Int32, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Int32
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A4E374E756D6265727337636865636B65644564] {{.*}} : $@convention(c) (Double, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Double
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A4E313553746174696346756E6374696F6E7337636865636B65644569] {{.*}} : $@convention(c) (Int32, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Int32
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A31306F7665726C6F6164656469] {{.*}} : $@convention(c) (Int32, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Int32
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A31306F7665726C6F6164656464] {{.*}} : $@convention(c) (Double, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Double
// SIL-DAG: sil shared {{.*}}[clang __swift_cxx_exception_5F5A3134616E6E6F7461746564466972737469] {{.*}} : $@convention(c) (Int32, Optional<UnsafeMutableRawPointer>, {{.*}}) -> Int32

// IR-DAG: define internal {{.*}} @_ZL{{[0-9]+}}__swift_cxx_exception_5F5A3133636865636B65644469766964656969{{.*}} personality
// IR-DAG: define internal {{.*}} @_ZL{{[0-9]+}}__swift_cxx_exception_5F5A4E313553746174696346756E6374696F6E7337636865636B65644569{{.*}} personality

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -target %target-swift-5.1-abi-triple -enable-testing -module-name Lib -package-name Package -emit-module -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-codesign %t/%target-library-name(Lib)

// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -Onone -I %t -package-name Package -L %t -lLib %t/main.swift -o %t/main-Onone %target-rpath(%t)
// RUN: %target-codesign %t/main-Onone
// RUN: %target-run %t/main-Onone %t/%target-library-name(Lib) | %FileCheck %s

// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -O -I %t -package-name Package -L %t -lLib %t/main.swift -o %t/main-O %target-rpath(%t)
// RUN: %target-codesign %t/main-O
// RUN: %target-run %t/main-O %t/%target-library-name(Lib) | %FileCheck %s

// REQUIRES: executable_test

//--- Lib.swift

struct InternalStruct {
  var stored = 0

  init() {}

  @export(implementation)
  func exportImpl() -> Int { 1 }

  @export(implementation)
  var property: Int {
    get { stored }
    set { stored = newValue }
  }

  @export(implementation)
  subscript(i: Int) -> Int { i }

  @export(implementation)
  func withDefault(_ x: Int = 4) -> Int { x }

  @export(implementation)
  func opaque() -> some Equatable { 5 }

  @export(implementation)
  var opaqueProperty: some Equatable { 6 }

  public struct NestedPublicStruct {
    public init() {}

    @export(implementation)
    public func method() -> Int { 7 }
  }
}

extension InternalStruct {
  @export(implementation)
  func extensionMethod() -> Int { 8 }
}

extension InternalStruct.NestedPublicStruct {
  @export(implementation)
  public func extensionMethod() -> Int { 9 }
}

struct InternalStructWithPackageMember {
  init() {}

  @export(implementation)
  package func method() -> Int { 10 }
}

//--- main.swift

@testable import Lib

var s = InternalStruct()
s.property = 2

// CHECK: 1 2 3 4 5 6 7 8 9 10
print(
  s.exportImpl(),
  s.property,
  s[3],
  s.withDefault(),
  s.opaque(),
  s.opaqueProperty,
  InternalStruct.NestedPublicStruct().method(),
  s.extensionMethod(),
  InternalStruct.NestedPublicStruct().extensionMethod(),
  InternalStructWithPackageMember().method())

// CHECK: true true
print(s.opaque() == s.opaque(), s.opaqueProperty == s.opaqueProperty)

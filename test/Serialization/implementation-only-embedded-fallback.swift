/// Test CheckImplementationOnlyStrict fallback errors at serialization.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module %t/HiddenLib.swift -o %t -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

/// Report errors on invalid references. Disable the early checks to trigger
/// underlying ones.
// RUN: not env SWIFT_DISABLE_IMPLICIT_CHECK_IMPLEMENTATION_ONLY=1 \
// RUN:   %target-swift-frontend -emit-module %t/MiddleLib.swift -o %t -I %t \
// RUN:   -D BROKEN \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CheckImplementationOnlyStrict 2> %t/out
// RUN: %FileCheck --input-file %t/out %t/MiddleLib.swift

/// Build a valid version of the library, skipping @_implementationOnly decls.
/// for a client to build against.
// RUN: %target-swift-frontend -emit-module %t/MiddleLib.swift -o %t -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CheckImplementationOnlyStrict

/// Build an actual client.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -enable-experimental-feature CheckImplementationOnlyStrict

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CheckImplementationOnlyStrict
// REQUIRES: embedded_stdlib_cross_compiling

//--- HiddenLib.swift

public struct A {}
public struct B {}
public struct C {}
public struct D {}
public struct E {}
public struct F {}

public struct OkA {}
public struct OkB {}
public struct OkC {}
public struct OkD {}
public struct OkE {}
public struct OkF {}

public protocol Proto {}
public protocol OkProto {}

//--- MiddleLib.swift

@_implementationOnly import HiddenLib

/// Referenced types

#if BROKEN
internal struct InternalStruct: Proto {
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'Proto'
  var a: A
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'A'
}

internal enum InternalEnum {
  case b(B)
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'B'
  case c(C)
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'C'
}

public class PublicClass {
  init() { fatalError() }
  internal var internalField: D
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'D'
  private var privateField: E
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'E'
}

@export(interface)
private func PrivateFunc(h: F) {}
// CHECK-DAG: error: invalid reference to implementation-only imported module 'HiddenLib' for 'F'
#endif

@_implementationOnly
internal struct OkInternalStruct: OkProto {
  var a: OkA
}

@_implementationOnly
internal struct NesterStruct {
  var a: OkA

  @_implementationOnly
  struct Nested {
    var b: OkB
  }
}

internal struct NesterStructB {
  @_implementationOnly
  struct Nested {
    var b: OkB
  }
}

@_implementationOnly
internal enum OkInternalEnum {
  case b(OkB)
  case c(OkC)
}

@_implementationOnly
internal class OkInternalClass {
  init() { fatalError() }
  internal var internalField: OkD
  private var privateField: OkE
}

@export(interface)
internal func OkPrivateFunc(h: OkF) {}

public struct PublicStruct {}

@export(interface)
public func PublicFunc() -> PublicStruct {
  let _: OkA
  return PublicStruct()
}

//--- Client.swift

import MiddleLib

let _ = PublicFunc()

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: asserts

/// Resilient scenario, we ignore underlying type of non-inlinable functions.
/// Build libraries.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface -verify

/// Build clients, with and without safety.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -enable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-access-control 2>&1 \
// RUN:   | %FileCheck %s

/// Build against the swiftinterface.
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck %s

/// Non-resilient scenario, all underlying types are loaded.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-module-path %t/Lib.swiftmodule -verify
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -verify -Xllvm -debug-only=Serialization \
// RUN:   -disable-deserialization-safety 2>&1 \
// RUN:   | %FileCheck --check-prefix=NON-RESILIENT %s
// NON-RESILIENT-NOT: Ignoring underlying information

//--- Lib.swift
public protocol V {}

public struct EV : V {
    public init () {}
}

@available(SwiftStdlib 5.1, *)
public extension V {
  private func referencedPrivateFunc(v: some V) -> some V { return v }

  /// Hidden underlying types.
// CHECK: Ignoring underlying information for opaque type of 'opaqueReferencingPrivate()'
  func opaqueReferencingPrivate() -> some V {
    referencedPrivateFunc(v: EV())
  }

// CHECK: Ignoring underlying information for opaque type of 'opaqueReferencingPrivateVar'
  var opaqueReferencingPrivateVar: some V {
    referencedPrivateFunc(v: EV())
  }

// CHECK: Ignoring underlying information for opaque type of 'opaqueReferencingPrivateVarPattern'
  var opaqueReferencingPrivateVarPattern: some V {
    get {
      referencedPrivateFunc(v: EV())
    }
  }

// CHECK: Ignoring underlying information for opaque type of 'subscript(_:)'
  subscript(v: some V) -> some V {
    referencedPrivateFunc(v: v)
  }

  /// Visible underlying types.
// CHECK: Loading underlying information for opaque type of 'inlinableOpaqueFunc()'
  @inlinable
  func inlinableOpaqueFunc() -> some V { EV() }

// CHECK: Loading underlying information for opaque type of 'aeicOpaqueFunc()'
  @_alwaysEmitIntoClient
  func aeicOpaqueFunc() -> some V { EV() }

// CHECK: Loading underlying information for opaque type of 'transparentOpaqueFunc()'
  @_transparent
  func transparentOpaqueFunc() -> some V { EV() }

// CHECK: Loading underlying information for opaque type of 'inlinableOpaqueVar'
  @inlinable
  var inlinableOpaqueVar: some V { EV() }

// CHECK: Loading underlying information for opaque type of 'inlinableOpaqueVarPattern'
  var inlinableOpaqueVarPattern: some V {
      @inlinable
      get { EV() }
  }
}

//--- Client.swift
import Lib

if #available(SwiftStdlib 5.1, *) {
  let v = EV()
  let _ = v.opaqueReferencingPrivate()
  let _ = v.opaqueReferencingPrivateVar
  let _ = v.opaqueReferencingPrivateVarPattern
  let _ = v[v]

  let _ = v.inlinableOpaqueFunc()
  let _ = v.aeicOpaqueFunc()
  let _ = v.transparentOpaqueFunc()

  let _ = v.inlinableOpaqueVar
  let _ = v.inlinableOpaqueVarPattern
}

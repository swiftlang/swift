// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// RUN: %FileCheck %t/src/A.swift < %t/A.swiftinterface

// Build the client using module
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -verify -module-name Client -I %t %t/src/Client.swift | %FileCheck %t/src/Client.swift

// REQUIRES: asserts

//--- A.swift
@frozen
public struct Inlinable {
  var _x: Int

// CHECK:      public var x: Swift.Int {
// CHECK-NEXT:    @usableFromInline
// CHECK-NEXT:    @storageRestrictions(initializes: _x) init
// CHECK-NEXT:    get
// CHECK-NEXT:  }

  public var x: Int {
    @usableFromInline
    @storageRestrictions(initializes: _x)
    init {
      self._x = newValue
    }

    get {
      _x
    }
  }

  @inlinable
  public init(x: Int) {
    self.x = x
  }
}

public struct Internal {
// CHECK:      public var y: Swift.Int {
// CHECK-NEXT:   get
// CHECK-NEXT: }

  public var y: Int {
    init {
    }

    get { 0 }
  }

  init(y: Int) {
    self.y = y
  }
}

@frozen
public struct Transparent {
   @usableFromInline
   var _x: Int

// CHECK:      public var x: Swift.Int {
// CHECK-NEXT:   @_alwaysEmitIntoClient @storageRestrictions(initializes: _x) init {
// CHECK-NEXT:     self._x = newValue
// CHECK-NEXT:   }
// CHECK-NEXT:   get
// CHECK-NEXT:  }

  public var x: Int {
    @_alwaysEmitIntoClient
    @storageRestrictions(initializes: _x)
    init {
      self._x = newValue
    }

    get {
      _x
    }
  }

  @_alwaysEmitIntoClient
  public init(x: Int) {
    self.x = x
  }
}

//--- Client.swift
import A

// CHECK-LABEL: sil hidden @$s6Client15testTransparentyyF : $@convention(thin) () -> ()
// CHECK: [[X:%.*]] = struct $Int (%1 : $Builtin.Int{{[0-9]+}})
// CHECK-NEXT: // function_ref Transparent.init(x:)
// CHECK-NEXT: [[TRANSPARENT_REF:%.*]] = function_ref @$s1A11TransparentV1xACSi_tcfC : $@convention(method) (Int, @thin Transparent.Type) -> Transparent
// CHECK-NEXT: apply [[TRANSPARENT_REF]]([[X]], %0) : $@convention(method) (Int, @thin Transparent.Type) -> Transparent
func testTransparent() {
  _ = Transparent(x: 42)
}

// CHECK-LABEL: sil shared @$s1A11TransparentV1xACSi_tcfC : $@convention(method) (Int, @thin Transparent.Type) -> Transparent

// CHECK-LABEL: sil hidden @$s6Client13testInlinableyyF : $@convention(thin) () -> ()
// CHECK: [[X:%.*]] = struct $Int (%1 : $Builtin.Int{{[0-9]+}})
// CHECK-NEXT: // function_ref Inlinable.init(x:)
// CHECK-NEXT: [[INLINABLE_REF:%.*]] = function_ref @$s1A9InlinableV1xACSi_tcfC : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable
// CHECK-NEXT: apply [[INLINABLE_REF]]([[X]], %0) : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable
func testInlinable() {
  _ = Inlinable(x: 42)
}

// CHECK-LABEL: sil @$s1A9InlinableV1xACSi_tcfC : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable

// CHECK-LABEL: sil shared @$s1A11TransparentV1xSivi : $@convention(thin) (Int, @thin Transparent.Type) -> @out Int

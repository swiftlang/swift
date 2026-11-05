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

// CHECK:      public var x: Swift::Int {
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
// CHECK:      public var y: Swift::Int {
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

// CHECK:      public var x: Swift::Int {
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

// CHECK-LABEL: sil shared [export_implementation] @$s1A11TransparentV1xACSi_tcfC : $@convention(method) (Int, @thin Transparent.Type) -> Transparent

// CHECK-LABEL: sil hidden @$s6Client13testInlinableyyF : $@convention(thin) () -> ()
// CHECK: [[X:%.*]] = struct $Int (%1 : $Builtin.Int{{[0-9]+}})
// CHECK-NEXT: // function_ref Inlinable.init(x:)
// CHECK-NEXT: [[INLINABLE_REF:%.*]] = function_ref @$s1A9InlinableV1xACSi_tcfC : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable
// CHECK-NEXT: apply [[INLINABLE_REF]]([[X]], %0) : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable
func testInlinable() {
  _ = Inlinable(x: 42)
}

// CHECK-LABEL: sil @$s1A9InlinableV1xACSi_tcfC : $@convention(method) (Int, @thin Inlinable.Type) -> Inlinable

// CHECK-LABEL: sil shared [export_implementation] @$s1A11TransparentV1xSivi : $@convention(thin) (Int, @thin Transparent.Type) -> @out Int

// MARK: Observation macro generation inspired tests

// An init accessor is printed only when the stored properties named by its
// '@storageRestrictions' are printed too.

// CHECK-LABEL: @frozen public struct FrozenStruct {
// CHECK-NEXT:    private var _value: Swift::Int
// CHECK-NEXT:    @usableFromInline
// CHECK-NEXT:    internal var value: Swift::Int {
// CHECK-NEXT:      @storageRestrictions(initializes: _value) init
// CHECK-NEXT:      get
// CHECK-NEXT:      set
// CHECK-NEXT:    }
// CHECK-NEXT:    @inlinable public init() { self.value = 0 }
// CHECK-NEXT:  }
@frozen
public struct FrozenStruct {
  private var _value: Int

  @usableFromInline
  var value: Int {
    @storageRestrictions(initializes: _value)
    init { _value = newValue }
    get { _value }
    set { _value = newValue }
  }

  @inlinable public init() { self.value = 0 }
}

// CHECK-LABEL: public struct ResilientStruct {
// CHECK-NEXT:    @usableFromInline
// CHECK-NEXT:    internal var value: Swift::Int {
// CHECK-NEXT:      get
// CHECK-NEXT:      set
// CHECK-NEXT:    }
// CHECK-NEXT:    public var explicitlyUsableFromInline: Swift::Int {
// CHECK-NEXT:      get
// CHECK-NEXT:    }
// CHECK-NEXT:    public init()
// CHECK-NEXT:  }
public struct ResilientStruct {
  private var _value: Int
  private var _other: Int

  @usableFromInline
  var value: Int {
    @storageRestrictions(initializes: _value)
    init { _value = newValue }
    get { _value }
    set { _value = newValue }
  }

  public var explicitlyUsableFromInline: Int {
    @usableFromInline
    @storageRestrictions(initializes: _other)
    init { _other = newValue }
    get { _other }
  }

  public init() {
    self.value = 0
    self.explicitlyUsableFromInline = 0
  }
}

// This is the shape the '@Observable' macro produces for a
// '@usableFromInline' property.
// CHECK-LABEL: final public class ResilientClass {
// CHECK-NEXT:    @usableFromInline
// CHECK-NEXT:    final internal var value: Swift::Int {
// CHECK-NEXT:      get
// CHECK-NEXT:      set
// CHECK-NEXT:    }
public final class ResilientClass {
  private var _value: Int

  @usableFromInline
  var value: Int {
    @storageRestrictions(initializes: _value)
    init { _value = newValue }
    get { _value }
    set { _value = newValue }
  }

  public init() { self.value = 0 }
}

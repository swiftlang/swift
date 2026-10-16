// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

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

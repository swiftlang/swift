//===--- Structural.swift -------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Structural protocol, used to support extensible
// library-defined automatic derived conformances. This API is not stable
// and subject to change.
//
// Please see forum discussion for more information about the proposal:
// https://forums.swift.org/t/automatic-requirement-satisfaction-in-plain-swift/37158
//
//===----------------------------------------------------------------------===//

import Swift

/// A type that can be converted to and from its structural representation.
public protocol Structural {
  /// A structural representation for `Self`.
  associatedtype StructuralRepresentation

  /// Creates an instance from the given structural representation.
  init(structuralRepresentation: StructuralRepresentation)

  /// A structural representation of `self`.
  var structuralRepresentation: StructuralRepresentation {
    get set
  }
}

/// Structural representation of a Swift struct.
public struct StructuralStruct<Properties> {
  public var type: Any.Type?
  public var properties: Properties

  public init(_ properties: Properties) {
    self.type = nil
    self.properties = properties
  }

  public init(_ type: Any.Type, _ properties: Properties) {
    self.type = type
    self.properties = properties
  }
}

/// Structural representation of a Swift property.
public struct StructuralProperty<Value> {
  public var name: String
  public var value: Value
  public var isMutable: Bool

  public init(_ value: Value) {
    self.name = ""
    self.value = value
    self.isMutable = false
  }

  public init(_ name: String, _ value: Value) {
    self.name = name
    self.value = value
    self.isMutable = false
  }

  public init(_ name: String, _ value: Value, isMutable: Bool) {
    self.name = name
    self.value = value
    self.isMutable = isMutable
  }
}

/// Structural representation of a Swift enum.
public struct StructuralEnum<Cases> {
  public var type: Any.Type?
  public var cases: Cases

  public init(_ cases: Cases) {
    self.type = nil
    self.cases = cases
  }

  public init(_ type: Any.Type, _ cases: Cases) {
    self.type = type
    self.cases = cases
  }
}

/// Structural representation of a Swift enum case.
public struct StructuralCase<RawValue, AssociatedValues> {
  public var name: String
  public var rawValue: RawValue
  public var associatedValues: AssociatedValues

  public init(_ rawValue: RawValue, _ associatedValues: AssociatedValues) {
    self.name = ""
    self.rawValue = rawValue
    self.associatedValues = associatedValues
  }

  public init(
    _ name: String, _ rawValue: RawValue, _ associatedValues: AssociatedValues
  ) {
    self.name = name
    self.rawValue = rawValue
    self.associatedValues = associatedValues
  }
}

/// Structural representation of a heterogeneous list cons cell.
public struct StructuralCons<Value, Next> {
  public var value: Value
  public var next: Next

  public init(_ value: Value, _ next: Next) {
    self.value = value
    self.next = next
  }
}

/// Structural representation of an empty heterogeneous list cons cell.
public struct StructuralEmpty {
  public init() {}
}

/// Structural representation of an alternative between either Left or Right.
public enum StructuralEither<Left, Right> {
  case left(Left)
  case right(Right)
}

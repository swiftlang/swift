//===--- VTable.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SILBridging

public struct VTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedVTable

  public init(bridged: BridgedVTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, NoReflectionChildren {
    public let bridged: BridgedVTableEntry

    public enum Kind {
      /// The vtable entry is for a method defined directly in this class.
      case normal
      /// The vtable entry is inherited from the superclass.
      case inherited
      /// The vtable entry is inherited from the superclass, and overridden in this class.
      case overridden
    }

    fileprivate init(bridged: BridgedVTableEntry) {
      self.bridged = bridged
    }

    public init(kind: Kind, isNonOverridden: Bool, methodDecl: DeclRef, implementation: Function) {
      let bridgedKind: BridgedVTableEntry.Kind
      switch kind {
        case .normal:     bridgedKind = .Normal
        case .inherited:  bridgedKind = .Inherited
        case .overridden: bridgedKind = .Override
      }
      self.bridged = BridgedVTableEntry.create(bridgedKind, isNonOverridden,
                                               methodDecl.bridged, implementation.bridged)
    }

    public var kind: Kind {
      switch bridged.getKind() {
        case .Normal:    return .normal
        case .Inherited: return .inherited
        case .Override:  return .overridden
        default: fatalError()
      }
    }

    public var isNonOverridden: Bool { bridged.isNonOverridden() }

    public var methodDecl: DeclRef { DeclRef(bridged: bridged.getMethodDecl()) }

    public var implementation: Function { bridged.getImplementation().function }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedTable: BridgedVTable
    public let count: Int
    
    init(vTable: VTable) {
      self.bridgedTable = vTable.bridged
      self.count = vTable.bridged.getNumEntries()
    }

    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }
    
    public subscript(_ index: Int) -> Entry {
      assert(index >= startIndex && index < endIndex)
      return Entry(bridged: bridgedTable.getEntry(index))
    }
  }

  public var entries: EntryArray { EntryArray(vTable: self) }

  public var `class`: ClassDecl { bridged.getClass().getAs(ClassDecl.self) }

  /// Returns the concrete class type if this is a specialized vTable.
  public var specializedClassType: Type? { bridged.getSpecializedClassType().typeOrNil }

  public var isSpecialized: Bool { specializedClassType != nil }

  /// A lookup for a specific method with O(1) complexity.
  public func lookup(method: DeclRef) -> Entry? {
    let bridgedEntryOrNil = bridged.lookupMethod(method.bridged)
    if bridgedEntryOrNil.hasEntry {
      return Entry(bridged: bridgedEntryOrNil.entry)
    }
    return nil
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
}

extension OptionalBridgedVTable {
  public var vTable: VTable? {
    if let table {
      return VTable(bridged: BridgedVTable(vTable: table))
    }
    return nil
  }
}

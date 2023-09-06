//===--- Utils.swift - some SIL utilities ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import SILBridging

// Need to export "Basic" to make `Basic.assert` available in the Optimizer module.
// Otherwise The Optimizer would fall back to Swift's assert implementation.
@_exported import Basic

//===----------------------------------------------------------------------===//
//                                 SIL Stage
//===----------------------------------------------------------------------===//

public enum SILStage: Int {
  /// "Raw" SIL, emitted by SILGen, but not yet run through guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Raw SIL does not have fully-constructed SSA and may contain undiagnosed
  /// dataflow errors.
  case raw

  /// Canonical SIL, which has been run through at least the guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Canonical SIL has stricter invariants than raw SIL. It must not contain
  /// dataflow errors, and some instructions must be canonicalized to simpler
  /// forms.
  case canonical

  /// Lowered SIL, which has been prepared for IRGen and will no longer
  /// be passed to canonical SIL transform passes.
  ///
  /// In lowered SIL, the SILType of all SILValues is its SIL storage
  /// type. Explicit storage is required for all address-only and resilient
  /// types.
  ///
  /// Generating the initial Raw SIL is typically referred to as lowering (from
  /// the AST). To disambiguate, refer to the process of generating the lowered
  /// stage of SIL as "address lowering".
  case lowered
}

//===----------------------------------------------------------------------===//
//                            Sequence Utilities
//===----------------------------------------------------------------------===//

/// Types conforming to `HasName` will be displayed by their name (instead of the
/// full object) in collection descriptions.
///
/// This is useful to make collections, e.g. of BasicBlocks or Functions, readable.
public protocol HasShortDescription {
  var shortDescription: String { get }
}

private struct CustomMirrorChild : CustomStringConvertible, NoReflectionChildren {
  public var description: String
  
  public init(description: String) { self.description = description }
}

/// Makes a Sequence's `description` and `customMirror` formatted like Array, e.g. [a, b, c].
public protocol FormattedLikeArray : Sequence, CustomStringConvertible, CustomReflectable {
}

extension FormattedLikeArray {
  /// Display a Sequence in an array like format, e.g. [a, b, c]
  public var description: String {
    "[" + map {
      if let named = $0 as? HasShortDescription {
        return named.shortDescription
      }
      return String(describing: $0)
    }.joined(separator: ", ") + "]"
  }
  
  /// The mirror which adds the children of a Sequence, similar to `Array`.
  public var customMirror: Mirror {
    // If the one-line description is not too large, print that instead of the
    // children in separate lines.
    if description.count <= 80 {
      return Mirror(self, children: [])
    }
    let c: [Mirror.Child] = map {
      let val: Any
      if let named = $0 as? HasShortDescription {
        val = CustomMirrorChild(description: named.shortDescription)
      } else {
        val = $0
      }
      return (label: nil, value: val)
    }
    return Mirror(self, children: c, displayStyle: .collection)
  }
}

/// RandomAccessCollection which bridges to some C++ array.
///
/// It fixes the default reflection for bridged random access collections, which usually have a
/// `bridged` stored property.
/// Conforming to this protocol displays the "real" children  not just `bridged`.
public protocol BridgedRandomAccessCollection : RandomAccessCollection, CustomReflectable {
}

extension BridgedRandomAccessCollection {
  public var customMirror: Mirror {
    Mirror(self, children: self.map { (label: nil, value: $0 as Any) })
  }
}

/// A Sequence which is not consuming and therefore behaves like a Collection.
///
/// Many sequences in SIL and the optimizer should be collections but cannot
/// because their Index cannot conform to Comparable. Those sequences conform
/// to CollectionLikeSequence.
///
/// For convenience it also inherits from FormattedLikeArray.
public protocol CollectionLikeSequence : FormattedLikeArray {
}

public extension CollectionLikeSequence {
  var isEmpty: Bool { !contains(where: { _ in true }) }
}

// Also make the lazy sequences a CollectionLikeSequence if the underlying sequence is one.

extension LazySequence : CollectionLikeSequence,
                         FormattedLikeArray, CustomStringConvertible, CustomReflectable
                         where Base: CollectionLikeSequence {}

extension FlattenSequence : CollectionLikeSequence,
                            FormattedLikeArray, CustomStringConvertible, CustomReflectable
                            where Base: CollectionLikeSequence {}

extension LazyMapSequence : CollectionLikeSequence,
                            FormattedLikeArray, CustomStringConvertible, CustomReflectable
                            where Base: CollectionLikeSequence {}

extension LazyFilterSequence : CollectionLikeSequence,
                               FormattedLikeArray, CustomStringConvertible, CustomReflectable
                               where Base: CollectionLikeSequence {}

//===----------------------------------------------------------------------===//
//                            String parsing
//===----------------------------------------------------------------------===//

public struct StringParser {
  private var s: Substring
  private let originalLength: Int
  
  private mutating func consumeWhitespace() {
    s = s.drop { $0.isWhitespace }
  }

  public init(_ string: String) {
    s = Substring(string)
    originalLength = string.count
  }
  
  mutating func isEmpty() -> Bool {
    consumeWhitespace()
    return s.isEmpty
  }

  public mutating func consume(_ str: String) -> Bool {
    consumeWhitespace()
    if !s.starts(with: str) { return false }
    s = s.dropFirst(str.count)
    return true
  }

  public mutating func consumeInt(withWhiteSpace: Bool = true) -> Int? {
    if withWhiteSpace {
      consumeWhitespace()
    }
    var intStr = ""
    s = s.drop {
      if $0.isNumber {
        intStr.append($0)
        return true
      }
      return false
    }
    return Int(intStr)
  }
  
  public mutating func consumeIdentifier() -> String? {
    consumeWhitespace()
    var name = ""
    s = s.drop {
      if $0.isLetter {
        name.append($0)
        return true
      }
      return false
    }
    return name.isEmpty ? nil : name
  }
  
  public func throwError(_ message: StaticString) throws -> Never {
    throw ParsingError(message: message, position: originalLength - s.count)
  }
}

public struct ParsingError : Error {
  public let message: StaticString
  public let position: Int
}

//===----------------------------------------------------------------------===//
//                            Bridging Utilities
//===----------------------------------------------------------------------===//

extension Array where Element == Value {
  public func withBridgedValues<T>(_ c: (BridgedValueArray) -> T) -> T {
    return self.withUnsafeBufferPointer { bufPtr in
      assert(bufPtr.count == self.count)
      return bufPtr.withMemoryRebound(to: BridgeValueExistential.self) { valPtr in
        return c(BridgedValueArray(base: valPtr.baseAddress, count: self.count))
      }
    }
  }
}


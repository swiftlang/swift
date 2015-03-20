//===--- Mirror.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// The thing with the default schema
public struct Mirror {
  public typealias Child = (label: String?, value: Any)
  public typealias Structure = AnyForwardCollection<Child>
  
  // public enum Schema {
  // case Struct, Class, Enum, Optional, Array, Dictionary, Set
  // }

  public typealias Schema = MirrorDisposition
  
  public init<
    C: CollectionType where C.Generator.Element == Child
  >(structure: C, schema: Schema? = nil) {
    self.structure = Structure(structure)
    self.schema = schema
  }

  public init<
    C: CollectionType
  >(_ unlabeledChildren: C, schema: Schema? = nil) {
    self.structure = Structure(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.schema = schema
  }

  public let structure: Structure
  public let schema: Schema?
}


extension Mirror {
  internal struct LegacyChildren : CollectionType {
    init(_ oldMirror: MirrorType) {
      self._oldMirror = oldMirror
    }
    var startIndex: Int { return 0 }
    var endIndex: Int { return _oldMirror.count }
    subscript(position: Int) -> Child {
      let (label, childMirror) = _oldMirror[position]
      return (label: label, value: childMirror.value)
    }
    func generate() -> IndexingGenerator<LegacyChildren> {
      return IndexingGenerator(self)
    }
    internal let _oldMirror: MirrorType
  }
  
  public init(_ oldMirror: MirrorType) {
    self.init(
      structure: LegacyChildren(oldMirror),
      schema: oldMirror.disposition)
  }
}

public protocol CustomReflectable {
  func reflect() -> Mirror
}

public func reflect(x: Any) -> Mirror {
  if let customized? = x as? CustomReflectable {
    return customized.reflect()
  }
  else {
    return Mirror(Swift.reflect(x))
  }
}

//===--- Addressing -------------------------------------------------------===//
public protocol MirrorPathType {}
extension IntMax : MirrorPathType {}
extension Int : MirrorPathType {}
extension String : MirrorPathType {}

/// Returns the first index `i` in `indices(domain)` such that
/// `predicate(domain[i])` is `true``, or `nil` if
/// `predicate(domain[i])` is `false` for all `i`.
///
/// Complexity: O(\ `count(domain)`\ )
public func find<
  C: CollectionType
>(domain: C, predicate: (C.Generator.Element)->Bool) -> C.Index? {
  for i in indices(domain) {
    if predicate(domain[i]) {
      return i
    }
  }
  return nil
}

extension Mirror {
  struct _Dummy : CustomReflectable {
    var mirror: Mirror
    func reflect() -> Mirror { return mirror }
  }
  
  public func descendant(
    first: MirrorPathType, _ rest: MirrorPathType...
  ) -> Any? {
    var result: Any = _Dummy(mirror: self)
    for e in [first] + rest {
      let structure = reflect(result).structure
      let position: Structure.Index
      if let label? = e as? String {
        position = find(structure) { $0.label == label } ?? structure.endIndex
      }
      else if let offset? = (e as? Int).map({ IntMax($0) }) ?? (e as? IntMax) {
        position = advance(structure.startIndex, offset, structure.endIndex)
      }
      else {
        _preconditionFailure(
          "Someone added a conformance to MirrorPathType; that privilege is reserved to the standard library")
      }
      if position == structure.endIndex { return nil }
      result = structure[position].value
    }
    return result
  }
}

//===--- Adapters ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public struct LabeledStructure<Element>
  : CollectionType, DictionaryLiteralConvertible {
  
  public init(dictionaryLiteral elements: (String, Element)...) {
    self.elements = elements
  }

  /// Construct a copy of `other`
  ///
  /// exists primarily so a Dictionary literal can be passed as an argument
  public init(_ other: LabeledStructure) {
    self.elements = other.elements
  }
  
  public init(_ elements: [(String, Element)]) {
    self.elements = elements
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return elements.endIndex }
  
  public subscript(position: Int) -> Mirror.Child {
    return (label: elements[position].0, value: elements[position].1)
  }

  public func generate() -> IndexingGenerator<LabeledStructure> {
    return IndexingGenerator(self)
  }
  
  internal let elements: [(String, Element)]
}

extension Mirror : DictionaryLiteralConvertible {
  typealias Key = String
  typealias Value = Any
  public init(dictionaryLiteral elements: (String, Any)...) {
    self.init(LabeledStructure(elements))
  }
  
  public init<Element>(
    _ structure: LabeledStructure<Element>, schema: Schema? = nil
  ) {
    self.structure = Structure(structure)
    self.schema = schema
  }
}

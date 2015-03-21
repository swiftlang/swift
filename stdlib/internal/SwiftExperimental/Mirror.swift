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
public struct Mirror {
  public typealias Child = (label: String?, value: Any)
  public typealias Children = AnyForwardCollection<Child>
  
  public enum Schema {
  case Struct, Class, Enum, Tuple, Optional, Collection, Dictionary, Set,
    ObjectiveCObject

    internal init?(legacy: MirrorDisposition) {
      switch legacy {
      case .Struct: self = .Struct
      case .Class: self = .Class
      case .Enum: self = .Enum
      case .Tuple: self = .Tuple
      case .Aggregate: return nil
      case .IndexContainer: self = .Collection
      case .KeyContainer: self = .Dictionary
      case .MembershipContainer: self = .Set
      case .Container: preconditionFailure("unused!")
      case .Optional: self = .Optional
      case .ObjCObject: self = .ObjectiveCObject
      }
    }
  }

  public init<
    C: CollectionType where C.Generator.Element == Child
  >(children: C, schema: Schema? = nil) {
    self.children = Children(children)
    self.schema = schema
  }

  public init<
    C: CollectionType
  >(unlabeledChildren: C, schema: Schema? = nil) {
    self.children = Children(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.schema = schema
  }

  public let children: Children
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
      children: LegacyChildren(oldMirror),
      schema: Schema(legacy: oldMirror.disposition))
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
      let children = reflect(result).children
      let position: Children.Index
      if let label? = e as? String {
        position = find(children) { $0.label == label } ?? children.endIndex
      }
      else if let offset? = (e as? Int).map({ IntMax($0) }) ?? (e as? IntMax) {
        position = advance(children.startIndex, offset, children.endIndex)
      }
      else {
        _preconditionFailure(
          "Someone added a conformance to MirrorPathType; that privilege is reserved to the standard library")
      }
      if position == children.endIndex { return nil }
      result = children[position].value
    }
    return result
  }
}

//===--- Adapters ---------------------------------------------------------===//
//===----------------------------------------------------------------------===//

public struct DictionaryLiteral<Key, Value>
  : CollectionType, DictionaryLiteralConvertible {
  
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.elements = elements
  }

  /// Construct a copy of `other`
  ///
  /// exists primarily so a Dictionary literal can be passed as an argument
  public init(_ other: DictionaryLiteral) {
    self.elements = other.elements
  }
  
  public init(elements: [(Key, Value)]) {
    self.elements = elements
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return elements.endIndex }

  typealias Element = (Key,Value)
  public subscript(position: Int) -> Element {
    return elements[position]
  }

  public func generate() -> IndexingGenerator<DictionaryLiteral> {
    return IndexingGenerator(self)
  }
  
  internal let elements: [(Key, Value)]
}

extension Mirror {
  typealias Key = String
  typealias Value = Any
  
  public init<Element>(
    children: DictionaryLiteral<String, Element>,
    schema: Schema? = nil
  ) {
    self.children = Children(
      lazy(children).map { Child(label: $0.0, value: $0.1) }
    )
    self.schema = schema
  }
}

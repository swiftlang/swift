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

/// Representation of the sub-structure and optional "schema" of any
/// arbitrary instance.
///
/// Describes the parts---such as stored properties, collection
/// elements, tuple elements, active enumeration cases---that make up
/// any given instance.  May also supply a "schema" property that
/// suggests how this structure might be interpreted.
///
/// Mirrors are used by playgrounds and the debugger.
public struct Mirror {
  /// An element of the reflected instance's structure.  The optional
  /// `label` may be used when appropriate, e.g. to represent the name
  /// of a stored property or of an active `enum` case, and will be
  /// used for lookup when `String`\ s are passed to the `descendant`
  /// method.
  public typealias Child = (label: String?, value: Any)

  /// The type used to represent sub-structure.
  ///
  /// Depending on your needs, you may find it useful to "upgrade"
  /// instances of this type to `AnyBidirectionalCollection` or
  /// `AnyRandomAccessCollection`.  For example, to display the last
  /// 20 children of a mirror if they can be accessed efficiently, you
  /// might write::
  ///
  ///   if let b? = AnyBidirectionalCollection(someMirror.children) {
  ///     for i in advance(b.endIndex, -20, b.startIndex)..<b.endIndex {
  ///        println(b[i])
  ///     }
  ///   }
  public typealias Children = AnyForwardCollection<Child>

  /// A suggestion of how a `Mirror`\ 's is to be interpreted.
  ///
  /// Playgrounds and the debugger will show a representation similar
  /// to the one used for instances of the kind indicated by the
  /// `Schema` case name when the `Mirror` is used for display.
  public enum Schema {
  case Struct, Class, Enum, Tuple, Optional, Collection, Dictionary, Set,
    ObjectiveCObject
  }

  /// Initialize with the given collection of `children` and optional
  /// `schema`.
  ///
  /// The traversal protocol modeled by `children`\ 's indices
  /// (`ForwardIndexType`, `BidirectionalIndexType`, or
  /// `RandomAccessIndexType`) is captured so that the resulting
  /// `Mirror`\ 's `children` may be upgraded later.  See the failable
  /// initializers of `AnyBidirectionalCollection` and
  /// `AnyRandomAccessCollection` for details.
  public init<
    C: CollectionType where C.Generator.Element == Child
  >(children: C, schema: Schema? = nil) {
    self.children = Children(children)
    self.schema = schema
  }

  /// Initialize with the given collection of child instances, each
  /// with a `nil` `label`, and optional `schema`.
  ///
  /// This initializer is especially useful for the mirrors of
  /// collections, e.g.::
  ///
  ///   extension MyArray : CustomReflectable {
  ///     func customReflect() -> Mirror 
  ///       return Mirror(unlabelledChildren: self, .Collection)
  ///     }
  ///   }
  ///
  /// The traversal protocol modeled by `children`\ 's indices
  /// (`ForwardIndexType`, `BidirectionalIndexType`, or
  /// `RandomAccessIndexType`) is captured so that the resulting
  /// `Mirror`\ 's `children` may be upgraded later.  See the failable
  /// initializers of `AnyBidirectionalCollection` and
  /// `AnyRandomAccessCollection` for details.
  public init<
    C: CollectionType
  >(unlabeledChildren: C, schema: Schema? = nil) {
    self.children = Children(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.schema = schema
  }

  /// A collection of `Child` elements describing the structure of the
  /// reflected instance.
  public let children: Children

  /// Suggests a display representation for the reflected instance.
  public let schema: Schema?
}

/// A type that explicitly supplies its own Mirror.
///
/// Instances of any type can be `reflect`\ 'ed upon, but if you are
/// not satisfied with the `Mirror` supplied for your type by default,
/// you can make it conform to `CustomReflectable` and return a custom
/// `Mirror`.
public protocol CustomReflectable {
  /// Return the `Mirror` for `self`.
  ///
  /// Note: if `Self` has value semantics, the `Mirror` should be
  /// unaffected by subsequent mutations of `self`.
  func customReflect() -> Mirror
}

/// Return the mirror that reflects upon the given instance.
///
/// If the dynamic type of instance conforms to `CustomReflectable`,
/// returns the result of calling its `customReflect` method.
/// Otherwise, returns a mirror synthesized for `instance` by the
/// language.
///
/// Note: If the dynamic type of instance has value semantics,
/// subsequent mutations of `instance` will not observable in
/// `Mirror`.  In general, though, the observability of such mutations
/// is unspecified.
public func reflect(instance: Any) -> Mirror {
  if let customized? = instance as? CustomReflectable {
    return customized.customReflect()
  }
  else {
    return Mirror(Swift.reflect(instance))
  }
}

//===--- Addressing -------------------------------------------------------===//

/// A protocol for legitimate arguments to `Mirror`\ 's `descendant`
/// method.
///
/// Do not declare new conformances to this protocol; they will not
/// work as expected.
public protocol MirrorPathType {}
extension IntMax : MirrorPathType {}
extension Int : MirrorPathType {}
extension String : MirrorPathType {}

extension Mirror {
  internal struct _Dummy : CustomReflectable {
    var mirror: Mirror
    func customReflect() -> Mirror { return mirror }
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

extension Mirror {
  //
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

//===--- Legacy MirrorType Support ----------------------------------------===//
extension Mirror.Schema {
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

//===--- General Utilities ------------------------------------------------===//
// having nothing really to do with Mirrors.  These should be
// separately reviewed.

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

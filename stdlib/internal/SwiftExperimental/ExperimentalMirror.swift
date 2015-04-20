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
import Swift

// These are just here pending review.
/// Return `true` iff `t0` is identical to `t1`
func == (t0: Any.Type, t1: Any.Type) -> Bool {
  return unsafeBitCast(t0, Int.self) == unsafeBitCast(t1, Int.self)
}

/// Return `false` iff `t0` is identical to `t1`
func != (t0: Any.Type, t1: Any.Type) -> Bool {
  return !(t0 == t1)
}

#if _runtime(_ObjC)
// FIXME: ExistentialCollection needs to be supported before this will work
// without the ObjC Runtime.

/// Representation of the sub-structure and optional "display style"
/// of any arbitrary instance.
///
/// Describes the parts---such as stored properties, collection
/// elements, tuple elements, or the active enumeration case---that
/// make up any given instance.  May also supply a "display style"
/// property that suggests how this structure might be rendered.
///
/// Mirrors are used by playgrounds and the debugger.
public struct Mirror {
  public enum SuperclassMirror : NilLiteralConvertible {
    public init(nilLiteral: ()) {
      self = .None
    }
  case Synthesized
  case Customized(()->Mirror)
  case None
  }


  /// Initialize a mirror that reflects upon the given `subject`.
  ///
  /// If the dynamic type of `subject` conforms to
  /// `CustomReflectable`, returns the result of calling its
  /// `customMirror` method.  Otherwise, returns a mirror synthesized
  /// for `subject` by the language.
  ///
  /// Note: If the dynamic type of `subject` has value semantics,
  /// subsequent mutations of `subject` will not observable in
  /// `Mirror`.  In general, though, the observability of such
  /// mutations is unspecified.
  public init(reflecting subject: Any) {
    if let customized? = subject as? CustomReflectable {
      self = customized.customMirror()
    }
    else {
      self = Mirror(Swift.reflect(subject))
    }
  }
  
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
  /// `DisplayStyle` case name when the `Mirror` is used for display.
  public enum DisplayStyle {
  case Struct, Class, Enum, Tuple, Optional, Collection, Dictionary, Set
  }

  static func _noSuperclassMirror() -> Mirror? { return nil }
  
  internal static func _superclassGenerator<T: Any>(
    subject: T, _ superclassMirror: SuperclassMirror
  ) -> ()->Mirror? {
    switch superclassMirror {
    case .None: return Mirror._noSuperclassMirror
    case .Synthesized: return {
        // Find the right legacy superclass mirror (inefficient, but works)
        
        // get a mirror for the most-derived type
        var legacyMirror = reflect(subject)
        // get the most-derived type
        var derivedType: AnyClass? = subject.dynamicType as? AnyClass

        // Walk up the chain of mirrors/classes until we find T.self
        while let (m?, t?) = (legacyMirror._legacySuperMirror(), derivedType) {
            if t == T.self {
              return Mirror(m)
            }
            legacyMirror = m
            derivedType = _getSuperclass(t)
          }
        return nil
      }
    case .Customized(let overridden): return {
        overridden()
      }
    }
  }
  
  /// Initialize with the given collection of `children` and optional
  /// `displayStyle`.
  ///
  /// The traversal protocol modeled by `children`\ 's indices
  /// (`ForwardIndexType`, `BidirectionalIndexType`, or
  /// `RandomAccessIndexType`) is captured so that the resulting
  /// `Mirror`\ 's `children` may be upgraded later.  See the failable
  /// initializers of `AnyBidirectionalCollection` and
  /// `AnyRandomAccessCollection` for details.
  public init<
    T, C: CollectionType where C.Generator.Element == Child
  >(
    _ subject: T,
    children: C,
    superclassMirror: SuperclassMirror = .Synthesized,
    displayStyle: DisplayStyle? = nil
  ) {
    self._createSuperclassMirror = Mirror._superclassGenerator(
      subject, superclassMirror)
      
    self.children = Children(children)
    self.displayStyle = displayStyle
  }

  /// Initialize with the given collection of `Child` instances, each
  /// with a `nil` `label`, and optional `displayStyle`.
  ///
  /// This initializer is especially useful for the mirrors of
  /// collections, e.g.::
  ///
  ///   extension MyArray : CustomReflectable {
  ///     func customMirror() -> Mirror 
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
    T, C: CollectionType
  >(
    _ subject: T,
    unlabeledChildren: C,
    superclassMirror: SuperclassMirror = .Synthesized,
    displayStyle: DisplayStyle? = nil
  ) {
    self._createSuperclassMirror = Mirror._superclassGenerator(
      subject, superclassMirror)
      
    self.children = Children(
      lazy(unlabeledChildren).map { Child(label: nil, value: $0) }
    )
    self.displayStyle = displayStyle
  }

  /// Initialize with labeled `children` and optional `displayStyle`.
  ///
  /// Pass a dictionary literal with `String` keys as the first
  /// argument.  Be aware that although an *actual* `Dictionary` is
  /// arbitrarily-ordered, the ordering of the `Mirror`\ 's `children`
  /// will exactly match that of the literal you pass.
  public init<T>(
    _ subject: T,
    children: DictionaryLiteral<String, Any>,
    superclassMirror: SuperclassMirror = .Synthesized,
    displayStyle: DisplayStyle? = nil
  ) {
    self._createSuperclassMirror = Mirror._superclassGenerator(
      subject, superclassMirror)
      
    self.children = Children(
      lazy(children).map { Child(label: $0.0, value: $0.1) }
    )
    self.displayStyle = displayStyle
  }
  
  /// A collection of `Child` elements describing the structure of the
  /// reflected subject.
  public let children: Children

  /// Suggests a display style for the reflected subject.
  public let displayStyle: DisplayStyle?

  public var superclassMirror: Mirror? {
    return _createSuperclassMirror()
  }

  internal let _createSuperclassMirror: ()->Mirror?
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
  func customMirror() -> Mirror
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
    func customMirror() -> Mirror { return mirror }
  }
  
  /// Return a specific descendant of the reflected subject, or `nil`
  /// if no such descendant exists.
  ///
  /// A `String` argument selects the first `Child` with a matching label.
  /// An integer argument *n* select the *n*\ th `Child`.  For example::
  ///
  ///   var d = reflect(x).descendant(1, "two", 3)
  ///
  /// is equivalent to:
  ///
  /// .. parsed-literal::
  ///
  ///   var d = nil
  ///   let children = reflect(x).children
  ///   let p0 = advance(children.startIndex, **1**, children.endIndex)
  ///   if p0 != children.endIndex {
  ///     let grandChildren = reflect(children[p0].value).children
  ///     SeekTwo: for g in grandChildren {
  ///       if g.label == **"two"** {
  ///         let greatGrandChildren = reflect(g.value).children
  ///         let p1 = advance(
  ///           greatGrandChildren.startIndex, **3**, 
  ///           greatGrandChildren.endIndex)
  ///         if p1 != endIndex { **d = greatGrandChildren[p1].value** }
  ///         break SeekTwo
  ///       }
  ///     }
  ///   }
  ///
  /// As you can see, complexity for each element of the argument list
  /// depends on the argument type and capabilities of the collection
  /// used to initialize the corresponding subject's parent's mirror.
  /// Each `String` argument results in a linear search.  In short,
  /// this function is suitable for exploring the structure of a
  /// `Mirror` in a REPL or playground, but don't expect it to be
  /// efficient.
  public func descendant(
    first: MirrorPathType, _ rest: MirrorPathType...
  ) -> Any? {
    var result: Any = _Dummy(mirror: self)
    for e in [first] + rest {
      let children = Mirror(reflecting: result).children
      let position: Children.Index
      if let label? = e as? String {
        position = _find(children) { $0.label == label } ?? children.endIndex
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

//===--- Legacy MirrorType Support ----------------------------------------===//
extension Mirror.DisplayStyle {
  /// Construct from a legacy `MirrorDisposition`
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
    case .ObjCObject: self = .Class
    }
  }
}

/*
internal func _hasType(instance: Any, type: Any.Type) -> Bool {
  return unsafeBitCast(instance.dynamicType, Int.self)
    == unsafeBitCast(type, Int.self) 
}

extension MirrorType {
  final internal func _legacySuperMirror() -> MirrorType? {
    if self.count > 0 {
      let childMirror = self[0].1
      if _hasType(childMirror, _ClassSuperMirror.self) {
        return childMirror
      }
    }
    return nil
  }
}
*/

extension Mirror {
  /// An adapter that represents a legacy `MirrorType`\ 's children as
  /// a `Collection` with integer `Index`.  Note that the performance
  /// characterstics of the underlying `MirrorType` may not be
  /// appropriate for random access!  To avoid this pitfall, convert
  /// mirrors to use the new style, which only present forward
  /// traversal in general.
  internal struct LegacyChildren : CollectionType {
    init(_ oldMirror: MirrorType) {
      self._oldMirror = oldMirror
    }

    var startIndex: Int {
      return _oldMirror._legacySuperMirror() == nil ? 0 : 1
    }

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

  /// Construct from a legacy `MirrorType`.  Note that the resulting
  /// `Mirror`\ 's `children` collection will always be upgradable to
  /// `AnyRandomAccessCollection` even if it doesn't exhibit
  /// appropriate performance. To avoid this pitfall, convert mirrors
  /// to use the new style, which only present forward traversal in
  /// general.
  internal init(_ oldMirror: MirrorType) {
    self._createSuperclassMirror = {
      oldMirror._legacySuperMirror().map { Mirror($0) }
    }
    self.children = Children(LegacyChildren(oldMirror))
    self.displayStyle = DisplayStyle(legacy: oldMirror.disposition)
  }
}

/// Returns the first index `i` in `indices(domain)` such that
/// `predicate(domain[i])` is `true``, or `nil` if
/// `predicate(domain[i])` is `false` for all `i`.
///
/// Complexity: O(\ `count(domain)`\ )
internal func _find<
  C: CollectionType
>(domain: C, predicate: (C.Generator.Element)->Bool) -> C.Index? {
  for i in indices(domain) {
    if predicate(domain[i]) {
      return i
    }
  }
  return nil
}

/*
//===--- QuickLooks -------------------------------------------------------===//

// this typealias implies renaming the existing QuickLookObject to
// PlaygroundQuickLook (since it is an enum, the use of the word
// "Object" is misleading).
public typealias PlaygroundQuickLook = QuickLookObject

extension PlaygroundQuickLook {
  /// Initialize for the given `subject`.
  ///
  /// If the dynamic type of `subject` conforms to
  /// `CustomPlaygroundQuickLookable`, returns the result of calling
  /// its `customPlaygroundQuickLook` method.  Otherwise, returns
  /// a `PlaygroundQuickLook` synthesized for `subject` by the
  /// language.  Note: in some cases the result may be
  /// `.Text(String(reflecting: subject))`.
  ///
  /// Note: If the dynamic type of `subject` has value semantics,
  /// subsequent mutations of `subject` will not observable in
  /// `Mirror`.  In general, though, the observability of such
  /// mutations is unspecified.
  public init(reflecting subject: Any) {
    if let customized? = subject as? CustomPlaygroundQuickLookable {
      self = customized.customPlaygroundQuickLook()
    }
    else {
      if let q? = Swift.reflect(subject).quickLookObject {
        self = q
      }
      else {
        self = .Text(String(reflecting: subject))
      }
    }
  }
}

/// A type that explicitly supplies its own PlaygroundQuickLook.
///
/// Instances of any type can be `reflect`\ 'ed upon, but if you are
/// not satisfied with the `Mirror` supplied for your type by default,
/// you can make it conform to `CustomReflectable` and return a custom
/// `Mirror`.
public protocol CustomPlaygroundQuickLookable {
  /// Return the `Mirror` for `self`.
  ///
  /// Note: if `Self` has value semantics, the `Mirror` should be
  /// unaffected by subsequent mutations of `self`.
  func customPlaygroundQuickLook() -> PlaygroundQuickLook
}

//===--- General Utilities ------------------------------------------------===//
// This component could stand alone, but is used in Mirror's public interface.

/// Represent the ability to pass a dictionary literal in function
/// signatures.
///
/// A function with a `DictionaryLiteral` parameter can be passed a
/// Swift dictionary literal without causing a `Dictionary` to be
/// created.  This capability can be especially important when the
/// order of elements in the literal is significant.
///
/// For example::
///
///   struct IntPairs {
///     var elements: [(Int, Int)]
///     init(_ pairs: DictionaryLiteral<Int,Int>) {
///       elements = Array(pairs)
///     }
///   }
///
///   let x = IntPairs([1:2, 1:1, 3:4, 2:1])
///   println(x.elements)  // [(1, 2), (1, 1), (3, 4), (2, 1)]
public struct DictionaryLiteral<Key, Value> : DictionaryLiteralConvertible {
  /// Store `elements`
  public init(dictionaryLiteral elements: (Key, Value)...) {
    self.elements = elements
  }
  internal let elements: [(Key, Value)]
}

/// `CollectionType` conformance that allows `DictionaryLiteral` to
/// interoperate with the rest of the standard library.
extension DictionaryLiteral : CollectionType {
  /// The position of the first element in a non-empty `DictionaryLiteral`.
  ///
  /// Identical to `endIndex` in an empty `DictionaryLiteral`.
  ///
  /// Complexity: O(1)
  public var startIndex: Int { return 0 }
  
  /// The `DictionaryLiteral`\ 's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// Complexity: O(1)
  public var endIndex: Int { return elements.endIndex }

  // FIXME: a typealias is needed to prevent <rdar://20248032>
  // why doesn't this need to be public?
  typealias Element = (Key, Value)

  /// Access the element indicated by `position`.
  ///
  /// Requires: `position >= 0 && position < endIndex`.
  ///
  /// Complexity: O(1)
  public subscript(position: Int) -> Element {
    return elements[position]
  }

  /// Return a *generator* over the elements of this *sequence*.  The
  /// *generator*\ 's next element is the first element of the
  /// sequence.
  ///
  /// Complexity: O(1)
  public func generate() -> IndexingGenerator<DictionaryLiteral> {
    return IndexingGenerator(self)
  }
}
*/
#endif

/*
extension String {  
  /// Initialize `self` with the textual representation of `instance`.
  ///
  /// * If `T` conforms to `Streamable`, the result is obtained by
  ///   calling `instance.writeTo(s)` on an empty string s.
  /// * Otherwise, if `T` conforms to `CustomStringConvertible`, the
  ///   result is `instance`\ 's `description`
  /// * Otherwise, if `T` conforms to `CustomDebugStringConvertible`,
  ///   the result is `instance`\ 's `debugDescription`
  /// * Otherwise, an unspecified result is supplied automatically by
  ///   the Swift standard library.
  ///
  /// See Also: `String.init<T>(reflecting: T)`
  public init<T>(_ instance: T) {
    self.init()
    print(instance, &self)
  }

  /// Initialize `self` with a detailed textual representation of
  /// `instance`, suitable for debugging.
  ///
  /// * If `T` conforms to `CustomDebugStringConvertible`, the result
  ///   is `instance`\ 's `debugDescription`
  ///
  /// * Otherwise, if `T` conforms to `CustomStringConvertible`, the result
  ///   is `instance`\ 's `description`
  ///
  /// * Otherwise, if `T` conforms to `Streamable`, the result is
  ///   obtained by calling `instance.writeTo(s)` on an empty string s.
  ///
  /// * Otherwise, an unspecified result is supplied automatically by
  ///   the Swift standard library.
  ///
  /// See Also: `String.init<T>(T)`
  public init<T>(reflecting instance: T) {
    self.init()
    debugPrint(instance, &self)
  }
}
*/

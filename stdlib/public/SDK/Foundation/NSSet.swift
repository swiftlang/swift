//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

extension Set {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSSet` will be copied to ensure that the copy can
  /// not be mutated by other code.
  fileprivate init(_cocoaSet: __shared _NSSet) {
    assert(_isBridgedVerbatimToObjectiveC(Element.self),
      "Set can be backed by NSSet _variantStorage only when the member type can be bridged verbatim to Objective-C")
    // FIXME: We would like to call CFSetCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFSetCreateCopy() copies dictionary contents
    // unconditionally, resulting in O(n) copies even for immutable dictionaries.
    //
    // <rdar://problem/20697680> CFSetCreateCopy() does not call copyWithZone:
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Set(
      _immutableCocoaSet:
        unsafeBitCast(_cocoaSet.copy(with: nil) as AnyObject, to: _NSSet.self))
  }
}

extension NSSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

extension NSOrderedSet : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

// Set<Element> is conditionally bridged to NSSet
extension Set : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSSet {
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject, to: NSSet.self)
  }

  public static func _forceBridgeFromObjectiveC(_ s: NSSet, result: inout Set?) {
    if let native =
      Set<Element>._bridgeFromObjectiveCAdoptingNativeStorageOf(s as AnyObject) {

      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Element.self) {
      result = Set<Element>(_cocoaSet: unsafeBitCast(s, to: _NSSet.self))
      return
    }

    if Element.self == String.self {
      // String and NSString have different concepts of equality, so
      // string-keyed NSSets may generate key collisions when bridged over to
      // Swift. See rdar://problem/35995647
      var set = Set(minimumCapacity: s.count)
      s.enumerateObjects({ (anyMember: Any, _) in
        let member = Swift._forceBridgeFromObjectiveC(
          anyMember as AnyObject, Element.self)
        // FIXME: Log a warning if `member` is already in the set.
        set.insert(member)
      })
      result = set
      return
    }

    // `Set<Element>` where `Element` is a value type may not be backed by
    // an NSSet.
    var builder = _SetBuilder<Element>(count: s.count)
    s.enumerateObjects({ (anyMember: Any, _) in
      builder.add(member: Swift._forceBridgeFromObjectiveC(
        anyMember as AnyObject, Element.self))
    })
    result = builder.take()
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSSet, result: inout Set?
  ) -> Bool {
    let anySet = x as Set<NSObject>
    if _isBridgedVerbatimToObjectiveC(Element.self) {
      result = Swift._setDownCastConditional(anySet)
      return result != nil
    }

    result = anySet as? Set
    return result != nil
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(_ s: NSSet?) -> Set {
    // `nil` has historically been used as a stand-in for an empty
    // set; map it to an empty set.
    if _slowPath(s == nil) { return Set() }

    var result: Set? = nil
    Set<Element>._forceBridgeFromObjectiveC(s!, result: &result)
    return result!
  }
}

extension NSSet : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to avoid infinite recursion during bridging
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(self as! Set<AnyHashable>)
  }
}

extension NSOrderedSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSSet {
  // - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSSet : ExpressibleByArrayLiteral {
  public required convenience init(arrayLiteral elements: Any...) {
    self.init(array: elements)
  }
}

extension NSOrderedSet : ExpressibleByArrayLiteral {
  public required convenience init(arrayLiteral elements: Any...) {
    self.init(array: elements)
  }
}

extension NSSet {
  /// Initializes a newly allocated set and adds to it objects from
  /// another given set.
  ///
  /// - Returns: An initialized objects set containing the objects from
  ///   `set`. The returned set might be different than the original
  ///   receiver.
  @nonobjc
  public convenience init(set anSet: __shared NSSet) {
    // FIXME(performance)(compiler limitation): we actually want to do just
    // `self = anSet.copy()`, but Swift does not have factory
    // initializers right now.
    let numElems = anSet.count
    let stride = MemoryLayout<Optional<UnsafeRawPointer>>.stride
    let alignment = MemoryLayout<Optional<UnsafeRawPointer>>.alignment
    let bufferSize = stride * numElems
    assert(stride == MemoryLayout<AnyObject>.stride)
    assert(alignment == MemoryLayout<AnyObject>.alignment)

    let rawBuffer = UnsafeMutableRawPointer.allocate(
      byteCount: bufferSize, alignment: alignment)
    defer {
      rawBuffer.deallocate()
      _fixLifetime(anSet)
    }
    let valueBuffer = rawBuffer.bindMemory(
     to: Optional<UnsafeRawPointer>.self, capacity: numElems)

    CFSetGetValues(anSet, valueBuffer)
    let valueBufferForInit = rawBuffer.assumingMemoryBound(to: AnyObject.self)
    self.init(objects: valueBufferForInit, count: numElems)
  }
}

extension NSSet : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as Set<NSObject>)
  }
}

extension Set: CVarArg {}

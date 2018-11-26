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

//===----------------------------------------------------------------------===//
// Arrays
//===----------------------------------------------------------------------===//

extension NSArray : ExpressibleByArrayLiteral {
  /// Create an instance initialized with `elements`.
  public required convenience init(arrayLiteral elements: Any...) {
    // Let bridging take care of it.
    self.init(array: elements)
  }
}

extension Array : _ObjectiveCBridgeable {

  /// Private initializer used for bridging.
  ///
  /// The provided `NSArray` will be copied to ensure that the copy can
  /// not be mutated by other code.
  internal init(_cocoaArray: NSArray) {
    _sanityCheck(_isBridgedVerbatimToObjectiveC(Element.self),
      "Array can be backed by NSArray only when the element type can be bridged verbatim to Objective-C")
    // FIXME: We would like to call CFArrayCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFArrayCreateCopy() copies array contents unconditionally,
    // resulting in O(n) copies even for immutable arrays.
    //
    // <rdar://problem/19773555> CFArrayCreateCopy() is >10x slower than
    // -[NSArray copyWithZone:]
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Array(
      _immutableCocoaArray:
        unsafeBitCast(_cocoaArray.copy() as AnyObject, to: _NSArrayCore.self))
  }

  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSArray {
    return unsafeBitCast(self._bridgeToObjectiveCImpl(), to: NSArray.self)
  }

  public static func _forceBridgeFromObjectiveC(
    _ source: NSArray,
    result: inout Array?
  ) {
    // If we have the appropriate native storage already, just adopt it.
    if let native =
        Array._bridgeFromObjectiveCAdoptingNativeStorageOf(source) {
      result = native
      return
    }

    if _fastPath(_isBridgedVerbatimToObjectiveC(Element.self)) {
      // Forced down-cast (possible deferred type-checking)
      result = Array(_cocoaArray: source)
      return
    }

    result = _arrayForceCast([AnyObject](_cocoaArray: source))
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ source: NSArray,
    result: inout Array?
  ) -> Bool {
    // Construct the result array by conditionally bridging each element.
    let anyObjectArr = [AnyObject](_cocoaArray: source)

    result = _arrayConditionalCast(anyObjectArr)
    return result != nil
  }

  public static func _unconditionallyBridgeFromObjectiveC(
    _ source: NSArray?
  ) -> Array {
    // `nil` has historically been used as a stand-in for an empty
    // array; map it to an empty array instead of failing.
    if _slowPath(source == nil) { return Array() }

    // If we have the appropriate native storage already, just adopt it.
    if let native =
        Array._bridgeFromObjectiveCAdoptingNativeStorageOf(source!) {
      return native
    }

    if _fastPath(_isBridgedVerbatimToObjectiveC(Element.self)) {
      // Forced down-cast (possible deferred type-checking)
      return Array(_cocoaArray: source!)
    }

    return _arrayForceCast([AnyObject](_cocoaArray: source!))
  }
}

extension NSArray : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to avoid infinite recursion during bridging
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(self as! Array<AnyHashable>)
  }
}

extension NSArray : Sequence {
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  final public func makeIterator() -> NSFastEnumerationIterator {
    return NSFastEnumerationIterator(self)
  }
}

/* TODO: API review
extension NSArray : Swift.Collection {
  final public var startIndex: Int {
    return 0
  }

  final public var endIndex: Int {
    return count
  }
}
 */

extension NSArray {
  // Overlay: - (instancetype)initWithObjects:(id)firstObj, ...
  public convenience init(objects elements: Any...) {
    self.init(array: elements)
  }
}

extension NSArray {
  /// Initializes a newly allocated array by placing in it the objects
  /// contained in a given array.
  ///
  /// - Returns: An array initialized to contain the objects in
  ///    `anArray``. The returned object might be different than the
  ///    original receiver.
  ///
  /// Discussion: After an immutable array has been initialized in
  /// this way, it cannot be modified.
  @nonobjc
  public convenience init(array anArray: NSArray) {
    self.init(array: anArray as Array)
  }
}

extension NSArray : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as [AnyObject])
  }
}

extension Array: CVarArg {}

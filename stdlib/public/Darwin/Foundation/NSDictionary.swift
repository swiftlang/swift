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
import _SwiftFoundationOverlayShims

@objc internal protocol _NSDictionaryEnumerationHack {
  @objc(enumerateKeysAndObjectsWithOptions:usingBlock:)
  func enumerateKeysAndObjects(
    options: Int,
    using block: @convention(block) (
    Unmanaged<AnyObject>,
    Unmanaged<AnyObject>,
    UnsafeMutablePointer<UInt8>
    ) -> Void)
}

extension NSDictionary {
  internal func _fastEnumerateKeysAndObjects(block:
    (AnyObject, AnyObject)->Bool) -> Bool {
    var success = true
    let fastSelf = unsafeBitCast(self, to: _NSDictionaryEnumerationHack.self)
    fastSelf.enumerateKeysAndObjects(options:0, using:
      { (anyKey, anyValue, stopPtr) in
        anyKey._withUnsafeGuaranteedRef { (anyKey) in
          anyValue._withUnsafeGuaranteedRef { (anyValue) in
            success = block(anyKey, anyValue)
            if !success {
              stopPtr.pointee = 1
            }
          }
        }
    })
    return success
  }
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

extension NSDictionary : ExpressibleByDictionaryLiteral {
  public required convenience init(
    dictionaryLiteral elements: (Any, Any)...
  ) {
    // FIXME: Unfortunate that the `NSCopying` check has to be done at runtime.
    self.init(
      objects: elements.map { $0.1 as AnyObject },
      forKeys: elements.map { $0.0 as AnyObject as! NSCopying },
      count: elements.count)
  }
}

extension Dictionary {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSDictionary` will be copied to ensure that the copy can
  /// not be mutated by other code.
  fileprivate init(_cocoaDictionary: __shared AnyObject) {
    assert(
      _isBridgedVerbatimToObjectiveC(Key.self) &&
      _isBridgedVerbatimToObjectiveC(Value.self),
      "Dictionary can be backed by NSDictionary storage only when both key and value are bridged verbatim to Objective-C")
    // FIXME: We would like to call CFDictionaryCreateCopy() to avoid doing an
    // objc_msgSend() for instances of CoreFoundation types.  We can't do that
    // today because CFDictionaryCreateCopy() copies dictionary contents
    // unconditionally, resulting in O(n) copies even for immutable dictionaries.
    //
    // <rdar://problem/20690755> CFDictionaryCreateCopy() does not call copyWithZone:
    //
    // The bug is fixed in: OS X 10.11.0, iOS 9.0, all versions of tvOS
    // and watchOS.
    self = Dictionary(
      _immutableCocoaDictionary: _cocoaDictionary.copy(with: nil) as AnyObject)
  }
}

// Dictionary<Key, Value> is conditionally bridged to NSDictionary
extension Dictionary : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSDictionary {
    return unsafeBitCast(_bridgeToObjectiveCImpl() as AnyObject,
                         to: NSDictionary.self)
  }
  
  @inline(__always)
  internal static func _bridgeElement<T>(_ anyKey:AnyObject, _ force: Bool) -> T? {
    if force {
      return Swift._forceBridgeFromObjectiveC(anyKey, T.self)
    } else {
      return  Swift._conditionallyBridgeFromObjectiveC(anyKey, T.self)
    }
  }
  
  internal static func _nonverbatimBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?,
    force: Bool
    ) -> Bool {
    let success:Bool
    var maybeKey:Key? = nil
    var maybeKeyString:String? = nil
    var maybeValue:Value? = nil
    // `Dictionary<Key, Value>` where either `Key` or `Value` is a value type
    // may not be backed by an NSDictionary.
    if Key.self == String.self {
      var builder = _DictionaryBuilder<String, Value>(count: x.count)
      success = x._fastEnumerateKeysAndObjects { (anyKey, anyValue) in
        maybeValue = _bridgeElement(anyValue, force)
	guard let value:Value = maybeValue else {
          return false
        }
        
	maybeKeyString = _bridgeElement(anyKey, force) 
        guard let key:String = maybeKeyString else {
          return false
        }
        // String and NSString have different concepts of equality, so
        // string-keyed NSDictionaries may generate key collisions when bridged
        // over to Swift. See rdar://problem/35995647
        if key._isKnownNFC {
          builder.add(
            key: key,
            value: value)
        } else {
          // FIXME: Log a warning if `dict` already had a value for `key`
          builder.add(
            possibleDuplicateKey: key,
            value: value)
        }
        return true
      }
      if success {
        result = unsafeBitCast(builder.take(), to: Dictionary.self)
      }
    } else {
      var builder = _DictionaryBuilder<Key, Value>(count: x.count)
      success = x._fastEnumerateKeysAndObjects { (anyKey, anyValue) in
        maybeValue = _bridgeElement(anyValue, force)
        guard let value:Value = maybeValue else {
          return false
        }

	maybeKey = _bridgeElement(anyKey, force)
        guard let key:Key = maybeKey else {
          return false
        }
        builder.add(
          key: key,
          value: value)
        return true
      }
      if success {
        result = builder.take()
      }
    }
    return success
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
  ) {
    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
        x as AnyObject) {
      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      result = [Key : Value](_cocoaDictionary: x)
      return
    }

    _ = _nonverbatimBridgeFromObjectiveC(x, result: &result, force: true)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
    ) -> Bool {
    
    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
      x as AnyObject) {
      result = native
      return true
    }
    
    return _nonverbatimBridgeFromObjectiveC(x, result: &result, force: false)
  }

  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(
    _ d: NSDictionary?
  ) -> Dictionary {
    // `nil` has historically been used as a stand-in for an empty
    // dictionary; map it to an empty dictionary.
    if _slowPath(d == nil) { return Dictionary() }

    var result: Dictionary? = nil
    _forceBridgeFromObjectiveC(d!, result: &result)
    return result!
  }
}

extension NSDictionary : _HasCustomAnyHashableRepresentation {
  // Must be @nonobjc to avoid infinite recursion during bridging
  @nonobjc
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(self as! Dictionary<AnyHashable, AnyHashable>)
  }
}

extension NSDictionary : Sequence {
  // FIXME: A class because we can't pass a struct with class fields through an
  // [objc] interface without prematurely destroying the references.
  final public class Iterator : IteratorProtocol {
    var _fastIterator: NSFastEnumerationIterator
    var _dictionary: NSDictionary {
      return _fastIterator.enumerable as! NSDictionary
    }

    public func next() -> (key: Any, value: Any)? {
      if let key = _fastIterator.next() {
        // Deliberately avoid the subscript operator in case the dictionary
        // contains non-copyable keys. This is rare since NSMutableDictionary
        // requires them, but we don't want to paint ourselves into a corner.
        return (key: key, value: _dictionary.object(forKey: key)!)
      }
      return nil
    }

    internal init(_ _dict: __shared NSDictionary) {
      _fastIterator = NSFastEnumerationIterator(_dict)
    }
  }

  // Bridging subscript.
  @objc
  public subscript(key: Any) -> Any? {
    @objc(__swift_objectForKeyedSubscript:)
    get {
      // Deliberately avoid the subscript operator in case the dictionary
      // contains non-copyable keys. This is rare since NSMutableDictionary
      // requires them, but we don't want to paint ourselves into a corner.
      return self.object(forKey: key)
    }
  }

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension NSMutableDictionary {
  // Bridging subscript.
  @objc override public subscript(key: Any) -> Any? {
    @objc(__swift_objectForKeyedSubscript:)
    get {
      return self.object(forKey: key)
    }
    @objc(__swift_setObject:forKeyedSubscript:)
    set {
      // FIXME: Unfortunate that the `NSCopying` check has to be done at
      // runtime.
      let copyingKey = key as AnyObject as! NSCopying
      if let newValue = newValue {
        self.setObject(newValue, forKey: copyingKey)
      } else {
        self.removeObject(forKey: copyingKey)
      }
    }
  }
}

extension NSDictionary {
  /// Initializes a newly allocated dictionary and adds to it objects from
  /// another given dictionary.
  ///
  /// - Returns: An initialized dictionary--which might be different
  ///   than the original receiver--containing the keys and values
  ///   found in `otherDictionary`.
  @objc(__swiftInitWithDictionary_NSDictionary:)
  public convenience init(dictionary otherDictionary: __shared NSDictionary) {
    // FIXME(performance)(compiler limitation): we actually want to do just
    // `self = otherDictionary.copy()`, but Swift does not have factory
    // initializers right now.
    let numElems = otherDictionary.count
    let stride = MemoryLayout<AnyObject>.stride
    let alignment = MemoryLayout<AnyObject>.alignment
    let singleSize = stride * numElems
    let totalSize = singleSize * 2
    assert(stride == MemoryLayout<NSCopying>.stride)
    assert(alignment == MemoryLayout<NSCopying>.alignment)

    // Allocate a buffer containing both the keys and values.
    let buffer = UnsafeMutableRawPointer.allocate(
      byteCount: totalSize, alignment: alignment)
    defer {
      buffer.deallocate()
      _fixLifetime(otherDictionary)
    }

    let valueBuffer = buffer.bindMemory(to: AnyObject.self, capacity: numElems)
    let buffer2 = buffer + singleSize

    __NSDictionaryGetObjects(otherDictionary, buffer, buffer2, numElems)

    let keyBufferCopying = buffer2.assumingMemoryBound(to: NSCopying.self)
    self.init(objects: valueBuffer, forKeys: keyBufferCopying, count: numElems)
  }
}

extension NSDictionary : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(reflecting: self as [NSObject : AnyObject])
  }
}

extension Dictionary: CVarArg {}

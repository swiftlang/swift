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

// We don't check for NSCopying here for performance reasons. We would
// just crash anyway, and NSMutableDictionary will still do that when
// it tries to call -copyWithZone: and it's not there
private func duckCastToNSCopying(_ x: Any) -> NSCopying {
  return _unsafeReferenceCast(x as AnyObject, to: NSCopying.self)
}

//===----------------------------------------------------------------------===//
// Dictionaries
//===----------------------------------------------------------------------===//

extension NSDictionary : ExpressibleByDictionaryLiteral {
  public required convenience init(
    dictionaryLiteral elements: (Any, Any)...
  ) {
    
    self.init(
      objects: elements.map { $0.1 as AnyObject },
      forKeys: elements.map { duckCastToNSCopying($0.0) },
      count: elements.count)
  }
}

extension Dictionary {
  /// Private initializer used for bridging.
  ///
  /// The provided `NSDictionary` will be copied to ensure that the copy can
  /// not be mutated by other code.
  fileprivate init(_cocoaDictionary: __shared NSDictionary) {
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
    return unsafeBitCast(_bridgeToObjectiveCImpl(),
                         to: NSDictionary.self)
  }

  /***
  Precondition: `buffer` points to a region of memory bound to `AnyObject`,
    with a capacity large enough to fit at least `index`+1 elements of type `T`
  
  _bridgeInitialize rebinds the `index`th `T` of `buffer` to `T`,
    and initializes it to `value`

  Note: *not* the `index`th element of `buffer`, since T and AnyObject may be
  different sizes. e.g. if T is String (2 words) then given a buffer like so:

  [object:AnyObject, object:AnyObject, uninitialized, uninitialized]

  `_bridgeInitialize(1, of: buffer, to: buffer[1] as! T)` will leave it as:

  [object:AnyObject, object:AnyObject, string:String]

  and `_bridgeInitialize(0, of: buffer, to: buffer[0] as! T)` will then leave:

  [string:String, string:String]

  Doing this in reverse order as shown above is required if T and AnyObject are
  different sizes. Here's what we get if instead of 1, 0 we did 0, 1:

  [object:AnyObject, object:AnyObject, uninitialized, uninitialized]
  [string:String, uninitialized, uninitialized]
  <segfault trying to treat the second word of 'string' as an AnyObject>

  Note: if you have retained any of the objects in `buffer`, you must release
  them separately, _bridgeInitialize will overwrite them without releasing them
  */
  @inline(__always)
  private static func _bridgeInitialize<T>(index:Int,
    of buffer: UnsafePointer<AnyObject>, to value: T) {
    let typedBase = UnsafeMutableRawPointer(mutating:
                      buffer).assumingMemoryBound(to: T.self)
    let rawTarget = UnsafeMutableRawPointer(mutating: typedBase + index)
    rawTarget.initializeMemory(as: T.self, repeating: value, count: 1)
  }

  @inline(__always)
  private static func _verbatimForceBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to: T.Type
  ) {
    //doesn't have to iterate in reverse because sizeof(T) == sizeof(AnyObject)
    for i in 0..<count {
      _bridgeInitialize(index: i, of: buffer, to: buffer[i] as! T)
    }
  }

  @inline(__always)
  private static func _verbatimBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to type: T.Type
  ) -> Int {
    var numUninitialized = count
    while numUninitialized > 0 {
      guard let bridged = buffer[numUninitialized - 1] as? T else {
        return numUninitialized
      }
      numUninitialized -= 1
      _bridgeInitialize(index: numUninitialized, of: buffer, to: bridged)
    }
    return numUninitialized
  }

  @inline(__always)
  private static func _nonVerbatimForceBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to: T.Type
  ) {
    for i in (0..<count).reversed() {
      let bridged = buffer[i] as! T
      _bridgeInitialize(index: i, of: buffer, to: bridged)
    }
  }
  
  @inline(__always)
  private static func _nonVerbatimBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to: T.Type
  ) -> Int {
    var numUninitialized = count
    while numUninitialized > 0 {
      guard let bridged = Swift._conditionallyBridgeFromObjectiveC(
        buffer[numUninitialized - 1], T.self)
        else {
        return numUninitialized
      }
      numUninitialized -= 1
      _bridgeInitialize(index: numUninitialized, of: buffer, to: bridged)
    }
    return numUninitialized
  }

  @inline(__always)
  private static func _forceBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to: T.Type
  ) {
    if _isBridgedVerbatimToObjectiveC(T.self) {
      _verbatimForceBridge(buffer, count: count, to: T.self)
    } else {
      _nonVerbatimForceBridge(buffer, count: count, to: T.self)
    }
  }
  
  @inline(__always)
  private static func _conditionallyBridge<T>(
    _ buffer: UnsafeMutablePointer<AnyObject>,
    count: Int,
    to: T.Type
  ) -> Bool {
    let numUninitialized:Int
    if _isBridgedVerbatimToObjectiveC(T.self) {
      numUninitialized = _verbatimBridge(buffer, count: count, to: T.self)
    } else {
      numUninitialized = _nonVerbatimBridge(buffer, count: count, to: T.self)
    }
    if numUninitialized == 0 {
      return true
    }
    let numInitialized = count - numUninitialized
    (UnsafeMutableRawPointer(mutating: buffer).assumingMemoryBound(to:
      T.self) + numUninitialized).deinitialize(count: numInitialized)
    return false
  }

  @_specialize(where Key == String, Value == Any)
  public static func _forceBridgeFromObjectiveC(
    _ d: NSDictionary,
    result: inout Dictionary?
  ) {
    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
        d as AnyObject) {
      result = native
      return
    }

    if _isBridgedVerbatimToObjectiveC(Key.self) &&
       _isBridgedVerbatimToObjectiveC(Value.self) {
      //Lazily type-checked on access
      result = [Key : Value](_cocoaDictionary: d)
      return
    }

    let keyStride = MemoryLayout<Key>.stride
    let valueStride = MemoryLayout<Value>.stride
    let objectStride = MemoryLayout<AnyObject>.stride

    //If Key or Value are smaller than AnyObject, a Dictionary with N elements
    //doesn't have large enough backing stores to hold the objects to be bridged
    //For now we just handle that case the slow way.
    if keyStride < objectStride || valueStride < objectStride {
      var builder = _DictionaryBuilder<Key, Value>(count: d.count)
      d.enumerateKeysAndObjects({ (anyKey: Any, anyValue: Any, _) in
        builder.add(
          key: anyKey as! Key,
          value: anyValue as! Value)
      })
      result = builder.take()
    } else {
      defer { _fixLifetime(d) }
    
      let numElems = d.count
      
      // String and NSString have different concepts of equality, so
      // string-keyed NSDictionaries may generate key collisions when bridged
      // over to Swift. See rdar://problem/35995647
      let handleDuplicates = (Key.self == String.self)
      
      result = Dictionary(_unsafeUninitializedCapacity: numElems,
        allowingDuplicates: handleDuplicates) { keys, vals in
        
        let objectKeys = UnsafeMutableRawPointer(mutating:
          keys.baseAddress!).assumingMemoryBound(to: AnyObject.self)
        let objectVals = UnsafeMutableRawPointer(mutating:
          vals.baseAddress!).assumingMemoryBound(to: AnyObject.self)

        //This initializes the first N AnyObjects of the Dictionary buffers.
        //Any unused buffer space is left uninitialized
        //This is fixed up in-place as we bridge elements, by _bridgeInitialize
        __NSDictionaryGetObjects(d, objectVals, objectKeys, numElems)

        _forceBridge(objectKeys, count: numElems, to: Key.self)
        _forceBridge(objectVals, count: numElems, to: Value.self)
        
        return numElems
      }
    }
  }
  
  @_specialize(where Key == String, Value == Any)
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSDictionary,
    result: inout Dictionary?
  ) -> Bool {

    if let native = [Key : Value]._bridgeFromObjectiveCAdoptingNativeStorageOf(
        x as AnyObject) {
      result = native
      return true
    }

    let keyStride = MemoryLayout<Key>.stride
    let valueStride = MemoryLayout<Value>.stride
    let objectStride = MemoryLayout<AnyObject>.stride

    //If Key or Value are smaller than AnyObject, a Dictionary with N elements
    //doesn't have large enough backing stores to hold the objects to be bridged
    //For now we just handle that case the slow way.
    if keyStride < objectStride || valueStride < objectStride {
      result = x as [NSObject : AnyObject] as? Dictionary
      return result != nil
    }

    defer { _fixLifetime(x) }
    
    let numElems = x.count
    var success = true
    
    // String and NSString have different concepts of equality, so
    // string-keyed NSDictionaries may generate key collisions when bridged
    // over to Swift. See rdar://problem/35995647
    let handleDuplicates = (Key.self == String.self)
    
    let tmpResult = Dictionary(_unsafeUninitializedCapacity: numElems,
      allowingDuplicates: handleDuplicates) { keys, vals in
      
      let objectKeys = UnsafeMutableRawPointer(mutating:
        keys.baseAddress!).assumingMemoryBound(to: AnyObject.self)
      let objectVals = UnsafeMutableRawPointer(mutating:
        vals.baseAddress!).assumingMemoryBound(to: AnyObject.self)

      //This initializes the first N AnyObjects of the Dictionary buffers.
      //Any unused buffer space is left uninitialized
      //This is fixed up in-place as we bridge elements, by _bridgeInitialize
      __NSDictionaryGetObjects(x, objectVals, objectKeys, numElems)

      success = _conditionallyBridge(objectKeys, count: numElems, to: Key.self)
      if success {
        success = _conditionallyBridge(objectVals,
                                       count: numElems, to: Value.self)
        if !success {
          (UnsafeMutableRawPointer(mutating: objectKeys).assumingMemoryBound(to:
            Key.self)).deinitialize(count: numElems)
        }
      }
      return success ? numElems : 0
    }
    
    result = success ? tmpResult : nil
    return success
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
  // NOTE: older runtimes had
  // _TtCE10FoundationCSo12NSDictionary8Iterator as the ObjC name. The
  // two must coexist, so it was renamed. The old name must not be used
  // in the new runtime.
  @_objcRuntimeName(_TtCE10FoundationCSo12NSDictionary9_Iterator)
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
      let copyingKey = duckCastToNSCopying(key)
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

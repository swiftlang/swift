// Prototype code to be added to an eventual StringStorage.swift

import Swift
import SwiftShims

protocol CopyConstructible {  }
extension CopyConstructible {
  init(_ me: Self) {
    self = me
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
@_versioned
final class _StringStorage<Element: UnsignedInteger>
 : /*_SwiftNativeNSString,*/ _NSStringCore, CopyConstructible {

  // NOTE: If you change these stored properties, edit the
  // definitions in GlobalObjects.h and GlobalObject.cpp
  var count: Int
  var capacity: Int // Should be a let.

  internal init(DoNotCallMe: ()) { count = 0; capacity = 0 }

  convenience init(count: Int, minimumCapacity: Int) {
    self.init(
      Builtin.allocWithTailElems_1(
      _StringStorage.self,
        Swift.max(count, minimumCapacity)._builtinWordValue, Element.self))

    let storageAddr = UnsafeMutableRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let endAddr = storageAddr + _swift_stdlib_malloc_size(storageAddr)
    let realCapacity = endAddr.assumingMemoryBound(to: Element.self)
      - storageAddr.assumingMemoryBound(to: Element.self)

    // All stored properties are uninitialized, but we can use assignment
    // because they're trivial types.
    self.count = count
    self.capacity = realCapacity
  }

  /// The empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  internal static var empty: _StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }


  @_versioned
  internal func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>)->R
  ) -> R {
    defer { _fixLifetime(self) }
    return body(
      UnsafeMutableBufferPointer(
        start: UnsafeMutablePointer(
          Builtin.projectTailElems(self, Element.self)),
        count: count
      )
    )
  }

  @_versioned
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    return withUnsafeMutableBufferPointer {
      body(UnsafeBufferPointer(start: UnsafePointer($0.baseAddress), count: count))
    }
  }
/*
}
// TODO: JIRA for error: @objc is not supported within extensions of generic classes
extension _StringStorage : _NSStringCore {
*/

  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(withUnsafeBufferPointer { $0[index] })
  }

  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }
    return UnsafeMutablePointer<UInt16>(
      Builtin.projectTailElems(self, Element.self))
  }

  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

extension _StringStorage : RandomAccessCollection, MutableCollection {
  var startIndex : Int { return 0 }
  var endIndex : Int { return count }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Element: UnsignedInteger> {
  internal var _storage: _StringStorage<Element>
}

extension _StringBuffer : RandomAccessCollection, MutableCollection {
  init(_ storage: _StringStorage<Element>) { _storage = storage }

  var startIndex : Int { return _storage.startIndex }
  var endIndex : Int { return _storage.endIndex }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return _storage[i]
    }
    set {
      _storage[i] = newValue
    }
  }
}

extension _StringBuffer : RangeReplaceableCollection {
  init() {
    self.init(_StringStorage.empty)
  }

  mutating func replaceSubrange<C: Collection>(
    _ target: Range<Index>, with source: C
  )
  where C.Iterator.Element == Element
  {
    let growth = numericCast(source.count) -
      distance(from: target.lowerBound, to: target.upperBound)

    let newCount = count + growth

    if _fastPath(newCount <= _storage.capacity) {
      _storage.withUnsafeMutableBufferPointer { elements in
        if growth > 0 {
          fatalError("implement me!")
        }
        else {
          fatalError("implement me!")
        }
      }
      return
    }
    else {
      fatalError("replace _storage with a new one that copies the elements")
    }
  }
}

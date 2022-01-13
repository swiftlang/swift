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

import SILBridging

//===----------------------------------------------------------------------===//
//                              Lists
//===----------------------------------------------------------------------===//

public protocol ListNode : AnyObject {
  associatedtype Element
  var next: Element? { get }
  var previous: Element? { get }
  
  /// The first node in the list. Used to implement `reversed()`.
  var _firstInList: Element { get }

  /// The last node in the list. Used to implement `reversed()`.
  var _lastInList: Element { get }
}

public struct List<NodeType: ListNode> :
      CollectionLikeSequence, IteratorProtocol where NodeType.Element == NodeType {
  private var currentNode: NodeType?
  
  public init(first: NodeType?) { currentNode = first }

  public mutating func next() -> NodeType? {
    if let node = currentNode {
      currentNode = node.next
      return node
    }
    return nil
  }

  public var first: NodeType? { currentNode }

  public func reversed() -> ReverseList<NodeType> {
    if let node = first {
      return ReverseList(first: node._lastInList)
    }
    return ReverseList(first: nil)
  }
}

public struct ReverseList<NodeType: ListNode> :
      CollectionLikeSequence, IteratorProtocol where NodeType.Element == NodeType {
  private var currentNode: NodeType?
  
  public init(first: NodeType?) { currentNode = first }

  public mutating func next() -> NodeType? {
    if let node = currentNode {
      currentNode = node.previous
      return node
    }
    return nil
  }

  public var first: NodeType? { currentNode }

  public func reversed() -> ReverseList<NodeType> {
    if let node = first {
      return ReverseList(first: node._firstInList)
    }
    return ReverseList(first: nil)
  }
}


//===----------------------------------------------------------------------===//
//                            Sequence Utilities
//===----------------------------------------------------------------------===//

/// Types conforming to `HasName` will be displayed by their name (instead of the
/// full object) in collection descriptions.
///
/// This is useful to make collections, e.g. of BasicBlocks or Functions, readable.
public protocol HasName {
  var name: String { get }
}

private struct CustomMirrorChild : CustomStringConvertible, CustomReflectable {
  public var description: String
  public var customMirror: Mirror { Mirror(self, children: []) }
  
  public init(description: String) { self.description = description }
}

/// Makes a Sequence's `description` and `customMirror` formatted like Array, e.g. [a, b, c].
public protocol FormattedLikeArray : Sequence, CustomStringConvertible, CustomReflectable {
}

extension FormattedLikeArray {
  /// Display a Sequence in an array like format, e.g. [a, b, c]
  public var description: String {
    "[" + map {
      if let named = $0 as? HasName {
        return named.name
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
      if let named = $0 as? HasName {
        val = CustomMirrorChild(description: named.name)
      } else {
        val = $0
      }
      return (label: nil, value: val)
    }
    return Mirror(self, children: c, displayStyle: .collection)
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
//                            Bridging Utilities
//===----------------------------------------------------------------------===//

extension BridgedStringRef {
  public var string: String {
    let buffer = UnsafeBufferPointer<UInt8>(start: data, count: Int(length))
    return String(decoding: buffer, as: UTF8.self)
  }

  func takeString() -> String {
    let str = string
    freeBridgedStringRef(self)
    return str
  }
}

extension String {
  public func withBridgedStringRef<T>(_ c: (BridgedStringRef) -> T) -> T {
    var str = self
    return str.withUTF8 { buffer in
      return c(BridgedStringRef(data: buffer.baseAddress, length: buffer.count))
    }
  }
}

extension Array where Element == Value {
  public func withBridgedValues<T>(_ c: (BridgedValueArray) -> T) -> T {
    return self.withUnsafeBytes { valPtr in
      assert(valPtr.count == self.count * 16)
      return c(BridgedValueArray(data: valPtr.baseAddress, count: self.count))
    }
  }
}

public typealias SwiftObject = UnsafeMutablePointer<BridgedSwiftObject>

extension UnsafeMutablePointer where Pointee == BridgedSwiftObject {
  init<T: AnyObject>(_ object: T) {
    let ptr = Unmanaged.passUnretained(object).toOpaque()
    self = ptr.bindMemory(to: BridgedSwiftObject.self, capacity: 1)
  }

  func getAs<T: AnyObject>(_ objectType: T.Type) -> T {
    return Unmanaged<T>.fromOpaque(self).takeUnretainedValue()
  }
}

extension Optional where Wrapped == UnsafeMutablePointer<BridgedSwiftObject> {
  func getAs<T: AnyObject>(_ objectType: T.Type) -> T? {
    if let pointer = self {
      return Unmanaged<T>.fromOpaque(pointer).takeUnretainedValue()
    }
    return nil
  }
}

extension BridgedArrayRef {
  func withElements<T, R>(ofType ty: T.Type, _ c: (UnsafeBufferPointer<T>) -> R) -> R {
    return data.withMemoryRebound(to: ty, capacity: numElements) { (ptr: UnsafePointer<T>) -> R in
      let buffer = UnsafeBufferPointer(start: ptr, count: numElements)
      return c(buffer)
    }
  }
}

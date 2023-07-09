//===--- Utils.swift - Some bridging utilities ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import BasicBridging
import SILBridging

import CxxStdlib

/// The assert function to be used in the compiler.
///
/// This overrides the standard Swift assert for two reasons:
/// * We also like to check for assert failures in release builds. Although this could be
///   achieved with `precondition`, it's easy to forget about it and use `assert` instead.
/// * We need to see the error message in crashlogs of release builds. This is even not the
///   case for `precondition`.
@_transparent
public func assert(_ condition: Bool, _ message: @autoclosure () -> String,
                   file: StaticString = #fileID, line: UInt = #line) {
  if !condition {
    fatalError(message(), file: file, line: line)
  }
}

/// The assert function (without a message) to be used in the compiler.
///
/// Unforuntately it's not possible to just add a default argument to `message` in the
/// other `assert` function. We need to defined this overload.
@_transparent
public func assert(_ condition: Bool, file: StaticString = #fileID, line: UInt = #line) {
  if !condition {
    fatalError("", file: file, line: line)
  }
}

//===----------------------------------------------------------------------===//
//                            Debugging Utilities
//===----------------------------------------------------------------------===//

/// Let's lldb's `po` command not print any "internal" properties of the conforming type.
///
/// This is useful if the `description` already contains all the information of a type instance.
public protocol NoReflectionChildren : CustomReflectable { }

public extension NoReflectionChildren {
  var customMirror: Mirror { Mirror(self, children: []) }
}


//===----------------------------------------------------------------------===//
//                              StringRef
//===----------------------------------------------------------------------===//

public struct StringRef : CustomStringConvertible, NoReflectionChildren {
  let _bridged: llvm.StringRef

  public init(bridged: llvm.StringRef) { self._bridged = bridged }

  public var string: String { _bridged.string }
  public var description: String { string }

  public var count: Int {
    Int(_bridged.__bytes_endUnsafe() - _bridged.__bytes_beginUnsafe())
  }

  public subscript(index: Int) -> UInt8 {
    let buffer = UnsafeBufferPointer<UInt8>(start: _bridged.__bytes_beginUnsafe(),
                                            count: count)
    return buffer[index]
  }

  public static func ==(lhs: StringRef, rhs: StaticString) -> Bool {
    let lhsBuffer = UnsafeBufferPointer<UInt8>(
      start: lhs._bridged.__bytes_beginUnsafe(),
      count: lhs.count)
    return rhs.withUTF8Buffer { (rhsBuffer: UnsafeBufferPointer<UInt8>) in
      if lhsBuffer.count != rhsBuffer.count { return false }
      return lhsBuffer.elementsEqual(rhsBuffer, by: ==)
    }
  }
  
  public static func !=(lhs: StringRef, rhs: StaticString) -> Bool { !(lhs == rhs) }

  public static func ~=(pattern: StaticString, value: StringRef) -> Bool { value == pattern }
}

//===----------------------------------------------------------------------===//
//                            Bridging Utilities
//===----------------------------------------------------------------------===//

extension llvm.StringRef {
  public var string: String {
    String(_cxxString: self.str())
  }
}

extension String {
  /// Underscored to avoid name collision with Swift LLVM Bindings.
  /// To be replaced with a bindings call once bindings are a dependency.
  public func _withStringRef<T>(_ c: (llvm.StringRef) -> T) -> T {
    var str = self
    return str.withUTF8 { buffer in
      return c(llvm.StringRef(buffer.baseAddress, buffer.count))
    }
  }

  /// Underscored to avoid name collision with the std overlay.
  /// To be replaced with an overlay call once the CI uses SDKs built with Swift 5.8.
  public init(_cxxString s: std.string) {
    self.init(cString: s.__c_strUnsafe())
    withExtendedLifetime(s) {}
  }
}

extension Array {
  public func withBridgedArrayRef<T>(_ c: (BridgedArrayRef) -> T) -> T {
    return withUnsafeBytes { buf in
      return c(BridgedArrayRef(data: buf.baseAddress!, numElements: count))
    }
  }
}

public typealias SwiftObject = UnsafeMutablePointer<BridgedSwiftObject>

extension UnsafeMutablePointer where Pointee == BridgedSwiftObject {
  public init<T: AnyObject>(_ object: T) {
    let ptr = unsafeBitCast(object, to: UnsafeMutableRawPointer.self)
    self = ptr.bindMemory(to: BridgedSwiftObject.self, capacity: 1)
  }

  public func getAs<T: AnyObject>(_ objectType: T.Type) -> T {
    return unsafeBitCast(self, to: T.self)
  }
}

extension Optional where Wrapped == UnsafeMutablePointer<BridgedSwiftObject> {
  public func getAs<T: AnyObject>(_ objectType: T.Type) -> T? {
    if let pointer = self {
      return pointer.getAs(objectType)
    }
    return nil
  }
}

extension BridgedArrayRef {
  public func withElements<T, R>(ofType ty: T.Type, _ c: (UnsafeBufferPointer<T>) -> R) -> R {
    let start = data?.bindMemory(to: ty, capacity: numElements);
    let buffer = UnsafeBufferPointer(start: start, count: numElements);
    return c(buffer)
  }
}

//===----------------------------------------------------------------------===//
//                            Sequence Utilities
//===----------------------------------------------------------------------===//

/// Types conforming to `HasName` will be displayed by their name (instead of the
/// full object) in collection descriptions.
///
/// This is useful to make collections, e.g. of BasicBlocks or Functions, readable.
public protocol HasShortDescription {
  var shortDescription: String { get }
}

private struct CustomMirrorChild : CustomStringConvertible, NoReflectionChildren {
  public var description: String
  
  public init(description: String) { self.description = description }
}

/// Makes a Sequence's `description` and `customMirror` formatted like Array, e.g. [a, b, c].
public protocol FormattedLikeArray : Sequence, CustomStringConvertible, CustomReflectable {
}

extension FormattedLikeArray {
  /// Display a Sequence in an array like format, e.g. [a, b, c]
  public var description: String {
    "[" + map {
      if let named = $0 as? HasShortDescription {
        return named.shortDescription
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
      if let named = $0 as? HasShortDescription {
        val = CustomMirrorChild(description: named.shortDescription)
      } else {
        val = $0
      }
      return (label: nil, value: val)
    }
    return Mirror(self, children: c, displayStyle: .collection)
  }
}

/// RandomAccessCollection which bridges to some C++ array.
///
/// It fixes the default reflection for bridged random access collections, which usually have a
/// `bridged` stored property.
/// Conforming to this protocol displays the "real" children  not just `bridged`.
public protocol BridgedRandomAccessCollection : RandomAccessCollection, CustomReflectable {
}

extension BridgedRandomAccessCollection {
  public var customMirror: Mirror {
    Mirror(self, children: self.map { (label: nil, value: $0 as Any) })
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
//                            String parsing
//===----------------------------------------------------------------------===//

public struct StringParser {
  private var s: Substring
  private let originalLength: Int
  
  private mutating func consumeWhitespace() {
    s = s.drop { $0.isWhitespace }
  }

  public init(_ string: String) {
    s = Substring(string)
    originalLength = string.count
  }
  
  mutating func isEmpty() -> Bool {
    consumeWhitespace()
    return s.isEmpty
  }

  public mutating func consume(_ str: String) -> Bool {
    consumeWhitespace()
    if !s.starts(with: str) { return false }
    s = s.dropFirst(str.count)
    return true
  }

  public mutating func consumeInt(withWhiteSpace: Bool = true) -> Int? {
    if withWhiteSpace {
      consumeWhitespace()
    }
    var intStr = ""
    s = s.drop {
      if $0.isNumber {
        intStr.append($0)
        return true
      }
      return false
    }
    return Int(intStr)
  }
  
  public mutating func consumeIdentifier() -> String? {
    consumeWhitespace()
    var name = ""
    s = s.drop {
      if $0.isLetter {
        name.append($0)
        return true
      }
      return false
    }
    return name.isEmpty ? nil : name
  }
  
  public func throwError(_ message: StaticString) throws -> Never {
    throw ParsingError(message: message, position: originalLength - s.count)
  }
}

public struct ParsingError : Error {
  public let message: StaticString
  public let position: Int
}

//===----------------------------------------------------------------------===//
//                            Bridging Utilities
//===----------------------------------------------------------------------===//

extension Array where Element == Value {
  public func withBridgedValues<T>(_ c: (BridgedValueArray) -> T) -> T {
    return self.withUnsafeBufferPointer { bufPtr in
      assert(bufPtr.count == self.count)
      return bufPtr.withMemoryRebound(to: BridgeValueExistential.self) { valPtr in
        return c(BridgedValueArray(base: valPtr.baseAddress, count: self.count))
      }
    }
  }
}


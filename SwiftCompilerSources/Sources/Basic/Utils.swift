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
import std

//===----------------------------------------------------------------------===//
//                              StringRef
//===----------------------------------------------------------------------===//

public struct StringRef : CustomStringConvertible, CustomReflectable {
  let _bridged: llvm.StringRef

  public init(bridged: llvm.StringRef) { self._bridged = bridged }

  public var string: String { _bridged.string }
  public var description: String { string }
  public var customMirror: Mirror { Mirror(self, children: []) }
  
  public static func ==(lhs: StringRef, rhs: StaticString) -> Bool {
    let lhsBuffer = UnsafeBufferPointer<UInt8>(
      start: lhs._bridged.__bytes_beginUnsafe(),
      count: Int(lhs._bridged.__bytes_endUnsafe() - lhs._bridged.__bytes_beginUnsafe()))
    return rhs.withUTF8Buffer { (rhsBuffer: UnsafeBufferPointer<UInt8>) in
      if lhsBuffer.count != rhsBuffer.count { return false }
      return lhsBuffer.elementsEqual(rhsBuffer, by: ==)
    }
  }
  
  public static func !=(lhs: StringRef, rhs: StaticString) -> Bool { !(lhs == rhs) }
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
    let ptr = Unmanaged.passUnretained(object).toOpaque()
    self = ptr.bindMemory(to: BridgedSwiftObject.self, capacity: 1)
  }

  public func getAs<T: AnyObject>(_ objectType: T.Type) -> T {
    return Unmanaged<T>.fromOpaque(self).takeUnretainedValue()
  }
}

extension Optional where Wrapped == UnsafeMutablePointer<BridgedSwiftObject> {
  public func getAs<T: AnyObject>(_ objectType: T.Type) -> T? {
    if let pointer = self {
      return Unmanaged<T>.fromOpaque(pointer).takeUnretainedValue()
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

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A C++ type that is an object that can refer to a contiguous sequence of objects.
///
/// C++ standard library type `std::span` conforms to this protocol.
public protocol CxxSpan<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger

  init()
  init(_ unsafePointer : UnsafePointer<Element>, _ count: Size)
}

extension CxxSpan {
  /// Creates a C++ span from a Swift UnsafeBufferPointer
  @inlinable
  public init(_ unsafeBufferPointer: UnsafeBufferPointer<Element>) {
    precondition(unsafeBufferPointer.baseAddress != nil, 
                  "UnsafeBufferPointer should not point to nil")
    self.init(unsafeBufferPointer.baseAddress!, Size(unsafeBufferPointer.count))
  }
}

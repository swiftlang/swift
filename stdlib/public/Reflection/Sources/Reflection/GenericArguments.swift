//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift
import _Runtime

@available(SwiftStdlib 5.9, *)
@frozen
public struct GenericArguments {
  @usableFromInline
  let argumentPointer: UnsafeRawPointer?
  
  @usableFromInline
  let numberOfArguments: Int
  
  @inlinable
  init(_ argumentPointer: UnsafeRawPointer?, _ numberOfArguments: Int) {
    self.argumentPointer = argumentPointer
    self.numberOfArguments = numberOfArguments
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericArguments: RandomAccessCollection {
  @inlinable
  public var startIndex: Int {
    0
  }
  
  @inlinable
  public var endIndex: Int {
    numberOfArguments
  }
  
  @inlinable
  public func index(after i: Int) -> Int {
    i + 1
  }
  
  @inlinable
  public subscript(_ position: Int) -> Type {
    precondition(position < endIndex)
    
    let start = argumentPointer.unsafelyUnwrapped
    let address = start + position * MemoryLayout<Type>.size
    return address.loadUnaligned(as: Type.self)
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericArguments: BidirectionalCollection {
  @inlinable
  public func index(before i: Int) -> Int {
    i - 1
  }
}

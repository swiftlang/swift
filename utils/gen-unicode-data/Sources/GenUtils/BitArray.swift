//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct BitArray {
  public var words: [UInt64]
  public var size: UInt16
  
  public init(size: Int) {
    self.words = .init(repeating: 0, count: (size + 63) / 64)
    self.size = UInt16(size)
  }
  
  public subscript(_ bit: Int) -> Bool {
    get {
      return words[bit / 64] & (1 << (bit % 64)) != 0
    }
    
    set {
      if newValue {
        words[bit / 64] |= 1 << (bit % 64)
      } else {
        words[bit / 64] &= ~(1 << (bit % 64))
      }
    }
  }
  
  public mutating func insert(_ bit: Int) -> Bool {
    let oldData = words[bit / 64]
    words[bit / 64] |= 1 << (bit % 64)
    return oldData & (1 << (bit % 64)) == 0
  }
}

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

struct Table<Element> {
  var elements: UnsafeMutableBufferPointer<Element?>?
  var overflow: [Int: Element]?
  let capacity: Int
  
  init(capacity: Int) {
    self.capacity = capacity
  }
  
  subscript(_ index: Int) -> Element? {
    get {
      if index < capacity {
        return elements?[index]
      } else {
        return overflow?[index]
      }
    }
    set {
      if index < capacity {
        if let elements {
          elements[index] = newValue
        } else {
          let newElements = UnsafeMutableBufferPointer<Element?>.allocate(capacity: capacity)
          newElements.initialize(repeating: nil)
          newElements[index] = newValue
          elements = newElements
        }
      } else {
        if let newValue, overflow == nil {
          overflow = [index : newValue]
        } else {
          overflow?[index] = newValue
        }
      }
    }
  }
  
  mutating func removeValue(forKey index: Int) -> Element? {
    if index < capacity {
      if let elements {
        defer { elements[index] = nil }
        return elements[index]
      }
    } else {
      return overflow?.removeValue(forKey: index)
    }
    return nil
  }
  
  mutating func removeAll() {
    elements?.deinitialize()
    elements?.deallocate()
    elements = nil
    overflow?.removeAll()
  }
}

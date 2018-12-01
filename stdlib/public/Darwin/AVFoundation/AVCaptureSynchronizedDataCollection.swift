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

@_exported import AVFoundation // Clang module
import Foundation


#if os(iOS)
@available(iOS, introduced: 11.0)
extension AVCaptureSynchronizedDataCollection : Sequence {
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }

  public struct Iterator : IteratorProtocol {
    internal var fastIterator: NSFastEnumerationIterator

    internal init(_ collection: AVCaptureSynchronizedDataCollection) {
      self.fastIterator = NSFastEnumerationIterator(collection)
    }

    public mutating func next() -> AVCaptureSynchronizedData? {
      guard let nextAny = fastIterator.next() else { return nil }
      return (nextAny as! AVCaptureSynchronizedData)
    }
  }
}

#endif

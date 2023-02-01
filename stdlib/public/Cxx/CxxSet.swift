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

public protocol CxxSet<Element> {
  associatedtype Element
  associatedtype Size: BinaryInteger

  func count(_ element: Element) -> Size
}

extension CxxSet {
  public func contains(_ element: Element) -> Bool {
    return count(element) > 0
  }
}

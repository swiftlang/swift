//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import NaturalLanguage
import Foundation

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
extension NLTokenizer {
  @nonobjc
  public func tokenRange(at index: String.Index) -> Range<String.Index> {
    let str = self.string ?? ""
    let characterIndex = index.encodedOffset
    let nsrange = self.__tokenRange(at:characterIndex)
    return Range(nsrange, in: str)!
  }

  @nonobjc
  public func enumerateTokens(in range: Range<String.Index>, using block: (Range<String.Index>, NLTokenizer.Attributes) -> Bool) {
    guard let str = self.string else { return }
    let nsrange = NSRange(range, in: str)
    self.__enumerateTokens(in: nsrange) { (tokenNSRange, attrs, stop) in
      if let tokenRange = Range(tokenNSRange, in:str) {
        let keepGoing = block(tokenRange, attrs)
        if (!keepGoing) {
          stop.pointee = true
        }
      }
    }
  }

  @nonobjc
  public func tokens(for range: Range<String.Index>) -> [Range<String.Index>] {
    var array:[Range<String.Index>] = []
    self.enumerateTokens(in: range) { (tokenRange, attrs) -> Bool in
      array.append(tokenRange)
      return true
    }
    return array
  }
}

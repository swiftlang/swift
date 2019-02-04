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
extension NLTagger {
  @nonobjc
  public func tokenRange(at index: String.Index, unit: NLTokenUnit) -> Range<String.Index> {
    let str = self.string ?? ""
    let characterIndex = index.encodedOffset
    let nsrange = self.__tokenRange(at: characterIndex, unit: unit)
    return Range(nsrange, in: str)!
  }

  @nonobjc
  public func tag(at index: String.Index, unit: NLTokenUnit, scheme: NLTagScheme) -> (NLTag?, Range<String.Index>) {
    let str = self.string ?? ""
    let characterIndex = index.encodedOffset
    let rangePointer = NSRangePointer.allocate(capacity: 1)
    rangePointer.initialize(to: NSMakeRange(0, 0))
    let tag = self.__tag(at: characterIndex, unit: unit, scheme: scheme, tokenRange: rangePointer)
    let range = Range(rangePointer.pointee, in: str)!
    rangePointer.deallocate()
    return (tag, range)
  }
  
  @nonobjc
  public func enumerateTags(in range: Range<String.Index>, unit: NLTokenUnit, scheme: NLTagScheme, options: NLTagger.Options = [], using block: (NLTag?, Range<String.Index>) -> Bool) {
    guard let str = self.string else { return }
    let nsrange = NSRange(range, in:str)
    self.__enumerateTags(in: nsrange, unit: unit, scheme: scheme, options: options) { (tag, tokenNSRange, stop) in
      if let tokenRange = Range(tokenNSRange, in: str) {
        let keepGoing = block(tag, tokenRange)
        if (!keepGoing) {
          stop.pointee = true
        }
      }
    }
  }

  @nonobjc
  public func tags(in range: Range<String.Index>, unit: NLTokenUnit, scheme: NLTagScheme, options: NLTagger.Options = []) -> [(NLTag?, Range<String.Index>)] {
    var array:[(NLTag?, Range<String.Index>)] = []
    self.enumerateTags(in: range, unit: unit, scheme: scheme, options: options) { (tag, tokenRange) -> Bool in
      array.append((tag, tokenRange))
      return true
    }
    return array
  }

  @nonobjc
  public func setLanguage(_ language: NLLanguage, range: Range<String.Index>) {
    guard let str = self.string else { return }
    let nsrange = NSRange(range, in: str)
    self.__setLanguage(language, range: nsrange)
  }
  
  @nonobjc
  public func setOrthography(_ orthography: NSOrthography, range: Range<String.Index>) {
    guard let str = self.string else { return }
    let nsrange = NSRange(range, in: str)
    self.__setOrthography(orthography, range: nsrange)
  }
}

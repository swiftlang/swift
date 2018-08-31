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
extension NLLanguageRecognizer {
  @nonobjc
  public var languageHints: [NLLanguage : Double] {
    get {
      return self.__languageHints.mapValues { $0.doubleValue }
    }
    set(newHints) {
      self.__languageHints = newHints.mapValues { NSNumber(value: $0) }
    }
  }

  @nonobjc
  public func languageHypotheses(withMaximum maxHypotheses: Int) -> [NLLanguage : Double] {
    return self.__languageHypotheses(withMaximum: maxHypotheses).mapValues { $0.doubleValue }
  }
}

//===--- Random.swift ------------------------------------------------------===//
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

import SwiftShims

public protocol RandomGenerator {
  func next<T : FixedWidthInteger>(_ type: T.Type) -> T
  func next<T : FixedWidthInteger>(_ type: T.Type, upperBound: T) -> T
}

public enum Random : RandomGenerator {
  case `default`

  @_inlineable
  public func next<T : FixedWidthInteger>(_ type: T.Type) -> T {
    var random: T = 0
    _swift_stdlib_random(&random, MemoryLayout<T>.size, _fatalErrorFlags())
    return random
  }

  @_inlineable
  public func next<T : FixedWidthInteger>(_ type: T.Type, upperBound: T) -> T {
    let range = T.max % upperBound
    var random: T = 0

    repeat {
      _swift_stdlib_random(&random, MemoryLayout<T>.size, _fatalErrorFlags())
    } while random >= range

    return random % upperBound
  }
}

public protocol RandomizableCollection : Collection {
  var random: Element { get }
  func random(using generator: RandomGenerator) -> Element
}

extension RandomizableCollection {
  @_inlineable
  public var random: Element {
    return self.random(using: Random.default)
  }
}

public protocol Randomizable {
  static var random: Self { get }
  static func random(using generator: RandomGenerator) -> Self
}

extension Randomizable {
  @_inlineable
  public static var random: Self {
    return self.random(using: Random.default)
  }
}

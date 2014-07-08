//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct ZipGenerator2<E0 : Generator, E1 : Generator> : Generator
{
  public typealias Element = (E0.Element,E1.Element)

  public init(_ e0: E0, _ e1: E1) {
    baseStreams = (e0,e1)
  }

  public mutating func next() -> Element? {
    var e0 = baseStreams.0.next()
    if !e0 { return .None }
    var e1 = baseStreams.1.next()
    if !e1 { return .None }
    return .Some((e0!, e1!))
  }

  var baseStreams : (E0,E1)
}

public struct Zip2<S0: Sequence, S1: Sequence> : Sequence
{
  public typealias Stream1 = S0.GeneratorType
  public typealias Stream2 = S1.GeneratorType
  public typealias GeneratorType = ZipGenerator2<Stream1, Stream2>

  public init(_ s0: S0, _ s1: S1) {
    sequences = (s0,s1)
  }

  public func generate() -> GeneratorType {
    return GeneratorType(
      sequences.0.generate(), 
      sequences.1.generate())
  }

  var sequences: (S0,S1)
}

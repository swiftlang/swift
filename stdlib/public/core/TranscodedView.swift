//===--- TranscodedView.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// TODO: description
public struct TranscodedView<FromEncoding: UnicodeEncoding,
                             FromCodeUnits: BidirectionalCollection,
                             ToEncoding: UnicodeEncoding>
  : BidirectionalCollection
  where FromCodeUnits.SubSequence.Index == FromCodeUnits.Index,
        FromCodeUnits.SubSequence.SubSequence.Index == FromCodeUnits.SubSequence.Index,
        FromCodeUnits.SubSequence.SubSequence.Iterator.Element == FromCodeUnits.SubSequence.Iterator.Element,
        FromCodeUnits.SubSequence: BidirectionalCollection,
        FromCodeUnits.SubSequence.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element
  // where FromCodeUnits.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element,
  //       FromCodeUnits.SubSequence == FromCodeUnits
  {

  // The original code units, encoded with FromEncoding
  public let underlyingCodeUnits: FromCodeUnits

  // The code unit offset from underlying code units collection
  public typealias Index = FromCodeUnits.Index

  // Requirements of Collection protocol:
  public var startIndex: Index {
    return underlyingCodeUnits.startIndex
  }
  public var endIndex: Index  {
    return underlyingCodeUnits.endIndex
  }
  // TODO: should the return type be a parse result? code units? scalar? buffer?
  public subscript(_ i: Index) -> ToEncoding.EncodedScalar {
    let (value, _) = parseForward(i)
    return ToEncoding.encode(value)!
  }
  public func index(after i: Index) -> Index {
    // TODO: better if there was a faster `scan1Forward` etc.
    let (_, next) = parseForward(i)
    return next
  }

  // Additionally, BidirectionalCollection requires:
  public func index(before i: Index) -> Index {
    // // TODO: better if there was a faster `scan1Forward` etc.
    let (_, prev) = parseBackward(i)
    return prev
  }

  public init(underlyingCodeUnits: FromCodeUnits) {
    self.underlyingCodeUnits = underlyingCodeUnits
  }

  // Initializer that allows clients to have labels on the types
  public init(of underlyingCodeUnits: FromCodeUnits,
       from: FromEncoding.Type,
       to: ToEncoding.Type) {
    self.init(underlyingCodeUnits: underlyingCodeUnits)
  }

  // Private helpers
  private func parseForward(_ i: Index)
    -> (FromEncoding.EncodedScalar, Index) {
    let slice = underlyingCodeUnits.suffix(from: i)
    let parseResult = FromEncoding.parse1Forward(slice, knownCount: 0)
    switch parseResult {
    case .valid(let value, let index):
      return (value, index)
    default:
      fatalError("FIXME: what to do now?")
    }
  }

  private func parseBackward(_ i: Index)
    -> (FromEncoding.EncodedScalar, Index) {
    let slice = underlyingCodeUnits.prefix(upTo: i)
    let parseResult = FromEncoding.parse1Reverse(slice, knownCount: 0)
    switch parseResult {
    case .valid(let value, let index):
      return (value, index)
    default:
      fatalError("FIXME: what to do now?")
    }
  }
}

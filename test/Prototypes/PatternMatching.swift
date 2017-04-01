//===--- PatternMatching.swift --------------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: rdar31343594

//===--- Niceties ---------------------------------------------------------===//
typealias Element_<S: Sequence> = S.Iterator.Element
extension Collection {
  func index(_ d: IndexDistance) -> Index {
    return index(startIndex, offsetBy: d)
  }
  func offset(of i: Index) -> IndexDistance {
    return distance(from: startIndex, to: i)
  }
}
//===--- Niceties ---------------------------------------------------------===//

enum MatchResult<Index: Comparable, MatchData> {
case found(end: Index, data: MatchData)
case notFound(resumeAt: Index?)
}

protocol Pattern {
  associatedtype Element : Equatable
  associatedtype Index : Comparable
  associatedtype MatchData = ()
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, MatchData>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
}

extension Pattern {
  func found<C: Collection>(in c: C) -> (extent: Range<Index>, data: MatchData)?
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var i = c.startIndex
    while i != c.endIndex {
      let m = self.matched(atStartOf: c[i..<c.endIndex])
      switch m {
      case .found(let end, let data):
        return (extent: i..<end, data: data)
      case .notFound(let j):
        i = j ?? c.index(after: i)
      }
    }
    return nil
  }
}

// FIXME: Using this matcher for found(in:) has worst-case performance
// O(pattern.count * c.count).
//
// Also implement one or more of
// KMP/Boyer-Moore[-Galil]/Sustik-Moore/Z-algorithm which run in O(pattern.count
// + c.count)
struct LiteralMatch<T: Collection, Index: Comparable> : Pattern
where Element_<T> : Equatable {
  typealias Element = Element_<T>
  init(_ pattern: T) { self.pattern = pattern }
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, ()>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var i = c.startIndex
    for p in pattern {
      if i == c.endIndex || c[i] != p {
        return .notFound(resumeAt: nil)
      }
      i = c.index(after: i)
    }
    return .found(end: i, data: ())
  }

  fileprivate let pattern: T
}

struct MatchAnyOne<T : Equatable, Index : Comparable> : Pattern {
  typealias Element = T
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, ()>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
  {
    return c.isEmpty
    ? .notFound(resumeAt: c.endIndex)
    : .found(end: c.index(after: c.startIndex), data: ())
  }
}

extension MatchAnyOne : CustomStringConvertible {
  var description: String { return "." }
}

enum MatchAny {}
var __ : MatchAny.Type { return MatchAny.self }
prefix func % <
  T : Equatable, Index : Comparable
>(_: MatchAny.Type) -> MatchAnyOne<T,Index> {
  return MatchAnyOne()
}

/// A matcher for two other matchers in sequence.
struct ConsecutiveMatches<M0: Pattern, M1: Pattern> : Pattern
where M0.Element == M1.Element, M0.Index == M1.Index {
  init(_ m0: M0, _ m1: M1) { self.matchers = (m0, m1) }
  fileprivate let matchers: (M0, M1)
  
  typealias Element = M0.Element
  typealias Index = M0.Index
  typealias MatchData = (midPoint: M0.Index, data: (M0.MatchData, M1.MatchData))

  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, MatchData>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var src0 = c[c.startIndex..<c.endIndex]
    while true {
      switch matchers.0.matched(atStartOf: src0) {
      case .found(let end0, let data0):
        switch matchers.1.matched(atStartOf: c[end0..<c.endIndex]) {
        case .found(let end1, let data1):
          return .found(end: end1, data: (midPoint: end0, data: (data0, data1)))
        case .notFound(_):
          if src0.isEmpty {
            // I don't think we can know anything interesting about where to
            // begin searching again, because there's no communication between
            // the two matchers that would allow it.
            return .notFound(resumeAt: nil)
          }
          // backtrack
          src0 = src0.dropLast()
        }
      case .notFound(let j):
        return .notFound(resumeAt: j)
      }
    }
  }
}

extension ConsecutiveMatches : CustomStringConvertible {
  var description: String { return "(\(matchers.0))(\(matchers.1))" }
}

struct RepeatMatch<M0: Pattern> : Pattern {
  typealias Element = M0.Element
  typealias MatchData = [(end: M0.Index, data: M0.MatchData)]
  
  let singlePattern: M0
  var repeatLimits: ClosedRange<Int>
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<M0.Index, MatchData>
  where C.Index == M0.Index, Element_<C> == M0.Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == M0.Element
  , C.SubSequence.Index == M0.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var lastEnd = c.startIndex
    var rest = c.dropFirst(0)
    var data: MatchData = []

  searchLoop:
    while !rest.isEmpty {
      switch singlePattern.matched(atStartOf: rest) {
      case .found(let x):
        data.append(x)
        lastEnd = x.end
        if data.count == repeatLimits.upperBound { break }
        rest = rest[x.end..<rest.endIndex]
      case .notFound(let r):
        if !repeatLimits.contains(data.count)  {
          return .notFound(resumeAt: r)
        }
        break searchLoop
      }
    }
    return .found(end: lastEnd, data: data)
  }
}

extension RepeatMatch : CustomStringConvertible {
  var description: String {
    let suffix: String
    switch (repeatLimits.lowerBound, repeatLimits.upperBound) {
    case (0, Int.max):
      suffix = "*"
    case (1, Int.max):
      suffix = "+"
    case (let l, Int.max):
      suffix = "{\(l)...}"
    default:
      suffix = "\(repeatLimits)"
    }
    return "(\(singlePattern))\(suffix)"
  }
}

enum OneOf<A, B> {
  case a(A)
  case b(B)
}

extension OneOf : CustomStringConvertible {
  var description: String {
    switch self {
    case .a(let x):
      return "\(x)"
    case .b(let x):
      return "\(x)"
    }
  }
}

struct MatchOneOf<M0: Pattern, M1: Pattern> : Pattern
where M0.Element == M1.Element, M0.Index == M1.Index {
  init(_ m0: M0, _ m1: M1) { self.matchers = (m0, m1) }
  fileprivate let matchers: (M0, M1)
  
  typealias Element = M0.Element
  typealias Index = M0.Index
  typealias MatchData = OneOf<M0.MatchData,M1.MatchData>

  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, MatchData>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence
  {
    switch matchers.0.matched(atStartOf: c) {
    case .found(let end, let data):
      return .found(end: end, data: .a(data))
    case .notFound(let r0):
      switch matchers.1.matched(atStartOf: c) {
      case .found(let end, let data):
        return .found(end: end, data: .b(data))
      case .notFound(let r1):
        if let s0 = r0, let s1 = r1 {
          return .notFound(resumeAt: min(s0, s1))
        }
        return .notFound(resumeAt: nil)
      }
    }
  }
}

extension MatchOneOf : CustomStringConvertible {
  var description: String { return "\(matchers.0)|\(matchers.1)" }
}


infix operator .. : AdditionPrecedence
postfix operator *
postfix operator +

func .. <M0: Pattern, M1: Pattern>(m0: M0, m1: M1) -> ConsecutiveMatches<M0,M1>
where M0.Element == M1.Element, M0.Index == M1.Index {
  return ConsecutiveMatches(m0, m1)
}

postfix func * <M: Pattern>(m: M) -> RepeatMatch<M> {
  return RepeatMatch(singlePattern: m, repeatLimits: 0...Int.max)
}

postfix func + <M: Pattern>(m: M) -> RepeatMatch<M> {
  return RepeatMatch(singlePattern: m, repeatLimits: 1...Int.max)
}

func | <M0: Pattern, M1: Pattern>(m0: M0, m1: M1) -> MatchOneOf<M0,M1>
where M0.Element == M1.Element, M0.Index == M1.Index {
  return MatchOneOf(m0, m1)
}

//===--- Just for testing -------------------------------------------------===//
struct MatchStaticString : Pattern {
  typealias Element = UTF8.CodeUnit
  typealias Buffer = UnsafeBufferPointer<Element>
  typealias Index = Buffer.Index

  let content: StaticString
  init(_ x: StaticString) { content = x }
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, ()>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence {
    return content.withUTF8Buffer {
      LiteralMatch<Buffer, Index>($0).matched(atStartOf: c)
    }
  }
}
extension MatchStaticString : CustomStringConvertible {
  var description: String { return String(describing: content) }
}

// A way to force string literals to be interpreted as StaticString
prefix operator %
extension StaticString {  
  static prefix func %(x: StaticString) -> MatchStaticString {
    return MatchStaticString(x)
  }
}

extension Collection where Iterator.Element == UTF8.CodeUnit {
  var u8str : String {
    var a = Array<UTF8.CodeUnit>()
    a.reserveCapacity(numericCast(count) + 1)
    a.append(contentsOf: self)
    a.append(0)
    return String(reflecting: String(cString: a))
  }
}

extension Pattern where Element == UTF8.CodeUnit {
  func searchTest<C: Collection>(
    in c: C,
    format: (MatchData)->String = { String(reflecting: $0) })
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence {
    print("searching for /\(self)/ in \(c.u8str)...", terminator: "")
    if let (extent, data) = self.found(in: c) {
      print(
        "\nfound at",
        "\(c.offset(of: extent.lowerBound)..<c.offset(of: extent.upperBound)):",
        c[extent].u8str,
        MatchData.self == Void.self ? "" : "\ndata: \(format(data))")
    }
    else {
      print("NOT FOUND")
    }
    print()
  }
}
//===--- Just for testing -------------------------------------------------===//

//===--- Tests ------------------------------------------------------------===//
let source = Array("the quick brown fox jumps over the lazy dog".utf8)

let source2 = Array("hack hack cough cough cough spork".utf8)

(%"fox").searchTest(in: source)
(%"fog").searchTest(in: source)
(%"fox" .. %" box").searchTest(in: source)
(%"fox" .. %" jump").searchTest(in: source)
(%"cough")*.searchTest(in: source2)
(%"sneeze")+.searchTest(in: source2)
(%"hack ")*.searchTest(in: source2)
(%"cough ")+.searchTest(in: source2)

// The final * steps around <rdar://29229409>
let fancyPattern
  = %"quick "..((%"brown" | %"black" | %"fox" | %"chicken") .. %" ")+ 
  .. (%__)* .. %"do"

fancyPattern.searchTest(in: source)

//===--- Parsing pairs ----------------------------------------------------===//
// The beginnings of what it will take to wrap and indent m.data in the end of
// the last test, to make it readable.
struct PairedStructure<I: Comparable> {
  let bounds: Range<I>
  let subStructure: [PairedStructure<I>]
}

struct Paired<T: Hashable, I: Comparable> : Pattern {
  typealias Element = T
  typealias Index = I
  typealias MatchData  = PairedStructure<I>
  
  let pairs: Dictionary<T,T>
  
  func matched<C: Collection>(atStartOf c: C) -> MatchResult<Index, MatchData>
  where C.Index == Index, Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == Index, C.SubSequence.SubSequence == C.SubSequence {
    guard let closer = c.first.flatMap({ pairs[$0] }) else {
      return .notFound(resumeAt: nil)
    }
    var subStructure: [PairedStructure<I>] = []
    var i = c.index(after: c.startIndex)
    var resumption: Index? = nil
    
    while i != c.endIndex {
      if let m = self.found(in: c[i..<c.endIndex]) {
        i = m.extent.upperBound
        subStructure.append(m.data)
        resumption = resumption ?? i
      }
      else {
        let nextI = c.index(after: i)
        if c[i] == closer {
          return .found(
            end: nextI,
            data: PairedStructure(
              bounds: c.startIndex..<nextI, subStructure: subStructure))
        }
        i = nextI
      }
    }
    return .notFound(resumeAt: resumption)
  }
}

// Local Variables:
// swift-syntax-check-fn: swift-syntax-check-single-file
// End:


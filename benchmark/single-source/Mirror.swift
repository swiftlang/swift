//===--- Mirror.swift ------------------------------------------------===//
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

// This test measures performance of Mirror and related things.
import TestsUtils

public let TypeName = BenchmarkInfo(
  name: "TypeName",
  runFunction: run_TypeName,
  tags: [.api, .String])

public let MirrorDefault = BenchmarkInfo(
  name: "MirrorDefault",
  runFunction: run_MirrorDefault,
  tags: [.api, .String])

struct S1 { var s: String; var d: Double }
struct S2 { var i: Int; var a: [Range<Int>] }

class C { var i: Int = 0 }
class D: C { var s: String = "" }

enum E {
  case a,b(Int)
}

struct G<T> { var t: T }
class H<T>: C { var t: T; init(_ t: T) { self.t = t }}

public func run_MirrorDefault(scale: Int) {
  let N = 100*scale
  
  let s1 = S1(s: "foo", d: 3.14)
  let s2 = S2(i: 42, a: [0..<4])
  let c = C()
  let d = D()
  let e = E.a
  let f = E.b(99)
  let g = G(t: 12.3)
  let h = H<[Int]>([1,2,3])

  var str = ""

  for _ in 0..<N {
    str = "\(s1),\(s2),\(c),\(d),\(e),\(f),\(g),\(h)"
    blackHole(str)
  }
  
  CheckResults(str ==
    "S1(s: \"foo\", d: 3.14),S2(i: 42, a: [Range(0..<4)]),Mirror.C,Mirror.D,a,b(99),G<Double>(t: 12.3),Mirror.H<Swift.Array<Swift.Int>>")
}

func typename<T>(of: T.Type) -> String {
  "\(T.self)"
}

public func run_TypeName(scale: Int) {
  let N = 1_000*scale
  var a: [String] = []
  a.reserveCapacity(16)

  for _ in 0..<N {
    a = []
    a.removeAll(keepingCapacity: true)
    a.append(typename(of: S1.self))
    a.append(typename(of: S2.self))
    a.append(typename(of: C.self))
    a.append(typename(of: D.self))
    a.append(typename(of: G<S1>.self))
    a.append(typename(of: G<C>.self))
    a.append(typename(of: G<String>.self))
    a.append(typename(of: H<Int>.self))
    a.append(typename(of: [S1].self))
    a.append(typename(of: [G<Int>].self))
    a.append(typename(of: [H<S1>].self))
    a.append(typename(of: S1?.self))
    a.append(typename(of: C?.self))
    blackHole(a)
  }

  let expected = ["S1",
    "S2",
    "C",
    "D",
    "G<S1>",
    "G<C>",
    "G<String>",
    "H<Int>",
    "Array<S1>",
    "Array<G<Int>>",
    "Array<H<S1>>",
    "Optional<S1>",
    "Optional<C>",
  ]
  CheckResults(a == expected)
}

//===--- FunctionConversion.swift -----------------------------------------===//
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
//

import StdlibUnittest


var FunctionConversionTestSuite = TestSuite("FunctionConversion")

protocol Quilt {
  var n: Int8 { get }
}

protocol Patchwork : Quilt {}

struct Trivial : Patchwork {
  let n: Int8
}

struct Loadable : Patchwork {
  let c: Fabric

  var n: Int8 {
    return c.n
  }

  init(n: Int8) {
    c = Fabric(n: n)
  }
}

struct AddrOnly : Patchwork {
  let a: Any

  var n: Int8 {
    return a as! Int8
  }

  init(n: Int8) {
    a = n
  }
}

class Fabric {
  let n: Int8

  init(n: Int8) {
    self.n = n
  }
}

class Parent : Patchwork {
  let n: Int8

  required init(n: Int8) {
    self.n = n
  }
}

class Child : Parent {}

func t1(s: Trivial?) -> Trivial {
  return Trivial(n: s!.n * 2)
}

func l1(s: Loadable?) -> Loadable {
  return Loadable(n: s!.n * 2)
}

func a1(s: AddrOnly?) -> AddrOnly {
  return AddrOnly(n: s!.n * 2)
}

FunctionConversionTestSuite.test("Optional") {
  let g11: (Trivial) -> Trivial? = t1
  let g12: (Trivial!) -> Trivial? = t1

  expectEqual(22, g11(Trivial(n: 11))?.n)
  expectEqual(24, g12(Trivial(n: 12))?.n)

  let g21: (Loadable?) -> Loadable? = l1
  let g22: (Loadable!) -> Loadable? = l1

  expectEqual(42, g21(Loadable(n: 21))?.n)
  expectEqual(44, g22(Loadable(n: 22))?.n)

  let g31: (AddrOnly?) -> AddrOnly? = a1
  let g32: (AddrOnly!) -> AddrOnly? = a1

  expectEqual(62, g31(AddrOnly(n: 31))?.n)
  expectEqual(64, g32(AddrOnly(n: 32))?.n)
}

func t2(s: Quilt) -> Trivial {
  return s as! Trivial
}

func t3(s: Quilt?) -> Trivial {
  return s! as! Trivial
}

func l2(s: Quilt) -> Loadable {
  return s as! Loadable
}

func l3(s: Quilt?) -> Loadable {
  return s! as! Loadable
}

func a2(s: Quilt) -> AddrOnly {
  return s as! AddrOnly
}

func a3(s: Quilt?) -> AddrOnly {
  return s! as! AddrOnly
}

FunctionConversionTestSuite.test("Existential") {
  let g11: (Trivial) -> Patchwork = t2
  let g12: (Trivial?) -> Patchwork = t3
  let g13: (Patchwork) -> Patchwork = t2

  expectEqual(11, g11(Trivial(n: 11)).n)
  expectEqual(12, g12(Trivial(n: 12)).n)
  expectEqual(13, g13(Trivial(n: 13)).n)

  let g21: (Loadable) -> Patchwork = l2
  let g22: (Loadable?) -> Patchwork = l3
  let g23: (Patchwork) -> Patchwork = l2

  expectEqual(21, g21(Loadable(n: 21)).n)
  expectEqual(22, g22(Loadable(n: 22)).n)
  expectEqual(23, g23(Loadable(n: 23)).n)

  let g31: (AddrOnly) -> Patchwork = a2
  let g32: (AddrOnly) -> Patchwork = a3
  let g33: (Patchwork) -> Patchwork = a2

  expectEqual(31, g31(AddrOnly(n: 31)).n)
  expectEqual(32, g32(AddrOnly(n: 32)).n)
  expectEqual(33, g33(AddrOnly(n: 33)).n)
}

func em(t: Quilt.Type?) -> Trivial.Type {
  return t! as! Trivial.Type
}

FunctionConversionTestSuite.test("ExistentialMetatype") {
  let g1: (Trivial.Type) -> Patchwork.Type = em
  let g2: (Trivial.Type?) -> Patchwork.Type = em
  let g3: (Patchwork.Type) -> Patchwork.Type = em
  let g4: (Patchwork.Type) -> Any = em

  let result1 = g1(Trivial.self)
  let result2 = g2(Trivial.self)
  let result3 = g3(Trivial.self)
  let result4 = g4(Trivial.self)

  expectEqual(true, result1 == Trivial.self)
  expectEqual(true, result2 == Trivial.self)
  expectEqual(true, result3 == Trivial.self)
  expectEqual(true, result4 as! Trivial.Type == Trivial.self)
}

func c1(p: Parent) -> (Child, Trivial) {
  return (Child(n: p.n), Trivial(n: p.n))
}

func c2(p: Parent?) -> (Child, Trivial) {
  return (Child(n: p!.n), Trivial(n: p!.n))
}

FunctionConversionTestSuite.test("ClassUpcast") {
  let g1: (Child) -> (Parent, Trivial?) = c1
  let g2: (Child) -> (Parent?, Trivial?) = c2

  expectEqual(g1(Child(n: 2)).0.n, 2)
  expectEqual(g2(Child(n: 4)).0!.n, 4)
}

func cm1(p: Parent.Type) -> (Child.Type, Trivial) {
  return (Child.self, Trivial(n: 0))
}

func cm2(p: Parent.Type?) -> (Child.Type, Trivial) {
  return (Child.self, Trivial(n: 0))
}

FunctionConversionTestSuite.test("ClassMetatypeUpcast") {
  let g1: (Child.Type) -> (Parent.Type, Trivial?) = cm1
  let g2: (Child.Type) -> (Parent.Type, Trivial?) = cm2
  let g3: (Child.Type?) -> (Parent.Type?, Trivial?) = cm2

  let result1 = g1(Child.self)
  let result2 = g2(Child.self)
  let result3 = g3(Child.self)

  expectEqual(true, result1.0 == Child.self)
  expectEqual(true, result2.0 == Child.self)
  expectEqual(true, result3.0! == Child.self)
}

func sq(i: Int) -> Int {
  return i * i
}

func f1(f: Any) -> (Int) -> Int {
  return f as! ((Int) -> Int)
}

FunctionConversionTestSuite.test("FuncExistential") {
  let g11: ((Int) -> Int) -> Any = f1

  expectEqual(100, f1(f: g11(sq))(10))
}

func generic1<T>(t: Parent) -> (T, Trivial) {
  return (t as! T, Trivial(n: 0))
}

func generic2<T : Parent>(f: @escaping (Parent) -> (T, Trivial), t: T) -> (Child) -> (Parent, Trivial?) {
  return f
}

FunctionConversionTestSuite.test("ClassArchetypeUpcast") {
  let g11: (Child) -> (Parent, Trivial?) = generic2(f: generic1, t: Child(n: 10))
  expectEqual(10, g11(Child(n: 10)).0.n)
}

func doesNotThrow() {}

FunctionConversionTestSuite.test("ThrowVariance") {
  let g: () throws -> () = doesNotThrow
  do { try print(g()) } catch {}
}

runAllTests()

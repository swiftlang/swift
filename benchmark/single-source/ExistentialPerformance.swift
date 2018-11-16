//===--- ExistentialPerformance.swift -------------------------*- swift -*-===//
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

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, change ExistentialPerformance.swift.gyb
// and run scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils

let t: [BenchmarkCategory] = []
let ta: [BenchmarkCategory] = [.api, .Array]

public let ExistentialPerformance: [BenchmarkInfo] = [
  BenchmarkInfo(name: "Existential.method.1x.Ref1",
    runFunction: run_method1xRef1, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Ref2",
    runFunction: run_method1xRef2, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Ref3",
    runFunction: run_method1xRef3, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Ref4",
    runFunction: run_method1xRef4, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Val0",
    runFunction: run_method1xVal0, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Val1",
    runFunction: run_method1xVal1, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Val2",
    runFunction: run_method1xVal2, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Val3",
    runFunction: run_method1xVal3, tags: t),
  BenchmarkInfo(name: "Existential.method.1x.Val4",
    runFunction: run_method1xVal4, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Ref1",
    runFunction: run_method2xRef1, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Ref2",
    runFunction: run_method2xRef2, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Ref3",
    runFunction: run_method2xRef3, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Ref4",
    runFunction: run_method2xRef4, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Val0",
    runFunction: run_method2xVal0, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Val1",
    runFunction: run_method2xVal1, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Val2",
    runFunction: run_method2xVal2, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Val3",
    runFunction: run_method2xVal3, tags: t),
  BenchmarkInfo(name: "Existential.method.2x.Val4",
    runFunction: run_method2xVal4, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref1",
    runFunction: run_Pass_method1xRef1, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref2",
    runFunction: run_Pass_method1xRef2, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref3",
    runFunction: run_Pass_method1xRef3, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref4",
    runFunction: run_Pass_method1xRef4, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val0",
    runFunction: run_Pass_method1xVal0, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val1",
    runFunction: run_Pass_method1xVal1, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val2",
    runFunction: run_Pass_method1xVal2, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val3",
    runFunction: run_Pass_method1xVal3, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val4",
    runFunction: run_Pass_method1xVal4, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref1",
    runFunction: run_Pass_method2xRef1, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref2",
    runFunction: run_Pass_method2xRef2, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref3",
    runFunction: run_Pass_method2xRef3, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref4",
    runFunction: run_Pass_method2xRef4, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val0",
    runFunction: run_Pass_method2xVal0, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val1",
    runFunction: run_Pass_method2xVal1, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val2",
    runFunction: run_Pass_method2xVal2, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val3",
    runFunction: run_Pass_method2xVal3, tags: t),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val4",
    runFunction: run_Pass_method2xVal4, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Ref1",
    runFunction: run_MutatingRef1, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Ref2",
    runFunction: run_MutatingRef2, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Ref3",
    runFunction: run_MutatingRef3, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Ref4",
    runFunction: run_MutatingRef4, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Val0",
    runFunction: run_MutatingVal0, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Val1",
    runFunction: run_MutatingVal1, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Val2",
    runFunction: run_MutatingVal2, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Val3",
    runFunction: run_MutatingVal3, tags: t),
  BenchmarkInfo(name: "Existential.Mutating.Val4",
    runFunction: run_MutatingVal4, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref1",
    runFunction: run_MutatingAndNonMutatingRef1, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref2",
    runFunction: run_MutatingAndNonMutatingRef2, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref3",
    runFunction: run_MutatingAndNonMutatingRef3, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref4",
    runFunction: run_MutatingAndNonMutatingRef4, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val0",
    runFunction: run_MutatingAndNonMutatingVal0, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val1",
    runFunction: run_MutatingAndNonMutatingVal1, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val2",
    runFunction: run_MutatingAndNonMutatingVal2, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val3",
    runFunction: run_MutatingAndNonMutatingVal3, tags: t),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val4",
    runFunction: run_MutatingAndNonMutatingVal4, tags: t),
  BenchmarkInfo(name: "Existential.Array.init.Ref1",
    runFunction: run_Array_initRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.init.Ref2",
    runFunction: run_Array_initRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.init.Ref3",
    runFunction: run_Array_initRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.init.Ref4",
    runFunction: run_Array_initRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.init.Val0",
    runFunction: run_Array_initVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.init.Val1",
    runFunction: run_Array_initVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.init.Val2",
    runFunction: run_Array_initVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.init.Val3",
    runFunction: run_Array_initVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.init.Val4",
    runFunction: run_Array_initVal4, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref1",
    runFunction: run_Array_method1xRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref2",
    runFunction: run_Array_method1xRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref3",
    runFunction: run_Array_method1xRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref4",
    runFunction: run_Array_method1xRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val0",
    runFunction: run_Array_method1xVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val1",
    runFunction: run_Array_method1xVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val2",
    runFunction: run_Array_method1xVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val3",
    runFunction: run_Array_method1xVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val4",
    runFunction: run_Array_method1xVal4, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref1",
    runFunction: run_Array_method2xRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref2",
    runFunction: run_Array_method2xRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref3",
    runFunction: run_Array_method2xRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref4",
    runFunction: run_Array_method2xRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val0",
    runFunction: run_Array_method2xVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val1",
    runFunction: run_Array_method2xVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val2",
    runFunction: run_Array_method2xVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val3",
    runFunction: run_Array_method2xVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val4",
    runFunction: run_Array_method2xVal4, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref1",
    runFunction: run_ArrayMutatingRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref2",
    runFunction: run_ArrayMutatingRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref3",
    runFunction: run_ArrayMutatingRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref4",
    runFunction: run_ArrayMutatingRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val0",
    runFunction: run_ArrayMutatingVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val1",
    runFunction: run_ArrayMutatingVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val2",
    runFunction: run_ArrayMutatingVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val3",
    runFunction: run_ArrayMutatingVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val4",
    runFunction: run_ArrayMutatingVal4, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref1",
    runFunction: run_ArrayShiftRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref2",
    runFunction: run_ArrayShiftRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref3",
    runFunction: run_ArrayShiftRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref4",
    runFunction: run_ArrayShiftRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.Shift.Val0",
    runFunction: run_ArrayShiftVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.Shift.Val1",
    runFunction: run_ArrayShiftVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.Shift.Val2",
    runFunction: run_ArrayShiftVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.Shift.Val3",
    runFunction: run_ArrayShiftVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.Shift.Val4",
    runFunction: run_ArrayShiftVal4, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref1",
    runFunction: run_ArrayConditionalShiftRef1, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref2",
    runFunction: run_ArrayConditionalShiftRef2, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref3",
    runFunction: run_ArrayConditionalShiftRef3, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref4",
    runFunction: run_ArrayConditionalShiftRef4, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val0",
    runFunction: run_ArrayConditionalShiftVal0, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val1",
    runFunction: run_ArrayConditionalShiftVal1, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val2",
    runFunction: run_ArrayConditionalShiftVal2, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val3",
    runFunction: run_ArrayConditionalShiftVal3, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val4",
    runFunction: run_ArrayConditionalShiftVal4, tags: ta, setUpFunction: caVal4),
]

// To exclude the setup overhead of existential array initialization,
// these are setup functions that **create array** for each variant type.
var array: [Existential]!
func ca<T: Existential>(_: T.Type) {
  array = initExistentialArray(withType: T.self, count: 128)
}
func caVal0() { ca(Val0.self) }
func caVal1() { ca(Val1.self) }
func caVal2() { ca(Val2.self) }
func caVal3() { ca(Val3.self) }
func caVal4() { ca(Val4.self) }
func caRef1() { ca(Ref1.self) }
func caRef2() { ca(Ref2.self) }
func caRef3() { ca(Ref3.self) }
func caRef4() { ca(Ref4.self) }
@inline(never)
func grabArray() -> [Existential] { // transfer array ownership to caller
  // FIXME: This is causing Illegal Instruction: 4 crash
  // defer { array = nil }
  // return array
  // This doesn't work either:
  // let a = array!
  // array = nil
  // return a
  return array!
}

protocol Existential {
  init()
  func doIt() -> Bool
  func reallyDoIt() -> Bool
  mutating func mutateIt() -> Bool
}

func next(_ x: inout Int, upto mod: Int) {
  x = (x + 1) % (mod + 1)
}

struct Val0 : Existential {
  func doIt() -> Bool {
    return true
	}
  func reallyDoIt() -> Bool {
    return true
  }
  mutating func mutateIt() -> Bool {
    return true
	}
}

struct Val1 : Existential {
	var f0: Int = 0

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
    return true
  }
  mutating func mutateIt() -> Bool {
    next(&f0, upto: 1)
    return true
	}
}

struct Val2 : Existential {
	var f0: Int = 0
	var f1: Int = 3

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
    return f0 == 0 && f1 == 3
  }
  mutating func mutateIt() -> Bool {
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    return true
	}
}

struct Val3 : Existential {
	var f0: Int = 0
	var f1: Int = 3
	var f2: Int = 7

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
    return f0 == 0 && f1 == 3 && f2 == 7
  }
  mutating func mutateIt() -> Bool {
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    next(&f2, upto: 7)
    return true
	}
}

struct Val4 : Existential {
	var f0: Int = 0
	var f1: Int = 3
	var f2: Int = 7
	var f3: Int = 13

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
    return f0 == 0 && f1 == 3 && f2 == 7 && f3 == 13
  }
  mutating func mutateIt() -> Bool {
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    next(&f2, upto: 7)
    next(&f3, upto: 13)
    return true
	}
}

class Klazz { // body same as Val2
  var f0: Int = 0
  var f1: Int = 3

  func doIt() -> Bool {
    return f0 == 0
	}
  func reallyDoIt() -> Bool {
   return f0 == 0 && f1 == 3
  }
  func mutateIt() -> Bool{
    next(&f0, upto: 1)
    next(&f1, upto: 3)
    return true
	}
}

struct Ref1 : Existential {
	var f0: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }
  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct Ref2 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }
  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct Ref3 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()
	var f2: Klazz = Klazz()

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }
  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}

struct Ref4 : Existential {
	var f0: Klazz = Klazz()
	var f1: Klazz = Klazz()
	var f2: Klazz = Klazz()
	var f3: Int = 0

  func doIt() -> Bool {
    return f0.doIt()
	}
  func reallyDoIt() -> Bool {
   return f0.reallyDoIt()
  }
  mutating func mutateIt() -> Bool{
    return f0.mutateIt()
  }
}


@inline(never)
func initExistential<T: Existential>(withType: T.Type) -> Existential {
  return T()
}

@inline(never)
func initExistentialArray<T: Existential>(withType: T.Type, count c: Int)
  -> [Existential] {
  return [T](repeating: T(), count: c)
}

@inline(never)
func passExistentialTwiceOneMethodCall(_ e0: Existential, _ e1: Existential)
  -> Bool {
  return e0.doIt() && e1.doIt()
}

@inline(never)
func passExistentialTwiceTwoMethodCalls(_ e0: Existential, _ e1: Existential)
  -> Bool {
  return e0.doIt() && e1.doIt() && e0.reallyDoIt() && e1.reallyDoIt()
}

func run_method1x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  for _ in 0 ..< N * 20_000 {
    if !existential.doIt() {
      fatalError("expected true")
    }
  }
}

func run_method2x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  for _ in 0 ..< N * 20_000 {
    if !existential.doIt()  || !existential.reallyDoIt() {
      fatalError("expected true")
    }
  }
}

func run_Pass_method1x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  let existential2 = initExistential(withType: T.self)
  for _ in 0 ..< N * 20_000 {
    if !passExistentialTwiceOneMethodCall(existential, existential2) {
      fatalError("expected true")
    }
  }
}

func run_Pass_method2x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existential = initExistential(withType: T.self)
  let existential2 = initExistential(withType: T.self)
  for _ in 0 ..< N * 20_000 {
    if !passExistentialTwiceTwoMethodCalls(existential, existential2) {
      fatalError("expected true")
    }
  }
}

func run_Mutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existential = initExistential(withType: T.self)
  for _ in 0 ..< N * 10_000 {
    if !existential.mutateIt()  {
      fatalError("expected true")
    }
  }
}

func run_MutatingAndNonMutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existential = initExistential(withType: T.self)
  for _ in 0 ..< N * 10_000 {
    let _ = existential.doIt()
    if !existential.mutateIt()  {
      fatalError("expected true")
    }
  }
}

func run_Array_init<T: Existential>(withType: T.Type, numberOfTimes N: Int) {

  for _ in 0 ..< N * 20 {
    blackHole(initExistentialArray(withType: T.self, count: 128))
  }
}

func run_Array_method1x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existentialArray = grabArray()
  for _ in 0 ..< N * 100 {
    for elt in existentialArray {
      if !elt.doIt()  {
        fatalError("expected true")
      }
    }
  }
}

func run_Array_method2x<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  let existentialArray = grabArray()
  for _ in 0 ..< N * 100 {
    for elt in existentialArray {
      if !elt.doIt() || !elt.reallyDoIt() {
        fatalError("expected true")
      }
    }
  }
}

func run_ArrayMutating<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = grabArray()
  for _ in 0 ..< N * 100 {
    for i in 0 ..< existentialArray.count {
      if !existentialArray[i].mutateIt()  {
        fatalError("expected true")
      }
    }
  }
}

func run_ArrayShift<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = grabArray()
  for _ in 0 ..< N * 10 {
    for i in 0 ..< existentialArray.count-1 {
      existentialArray.swapAt(i, i+1)
    }
  }
}

func run_ArrayConditionalShift<T: Existential>(withType: T.Type, numberOfTimes N: Int) {
  var existentialArray = grabArray()
  for _ in 0 ..< N * 10 {
    for i in 0 ..< existentialArray.count-1 {
      let curr = existentialArray[i]
      if curr.doIt() {
        existentialArray[i] = existentialArray[i+1]
        existentialArray[i+1] = curr
      }
    }
  }
}

// method.1x.
public func run_method1xVal0(_ N: Int) {
  run_method1x(withType: Val0.self, numberOfTimes: N)
}
public func run_method1xVal1(_ N: Int) {
  run_method1x(withType: Val1.self, numberOfTimes: N)
}
public func run_method1xVal2(_ N: Int) {
  run_method1x(withType: Val2.self, numberOfTimes: N)
}
public func run_method1xVal3(_ N: Int) {
  run_method1x(withType: Val3.self, numberOfTimes: N)
}
public func run_method1xVal4(_ N: Int) {
  run_method1x(withType: Val4.self, numberOfTimes: N)
}
public func run_method1xRef1(_ N: Int) {
  run_method1x(withType: Ref1.self, numberOfTimes: N)
}
public func run_method1xRef2(_ N: Int) {
  run_method1x(withType: Ref2.self, numberOfTimes: N)
}
public func run_method1xRef3(_ N: Int) {
  run_method1x(withType: Ref3.self, numberOfTimes: N)
}
public func run_method1xRef4(_ N: Int) {
  run_method1x(withType: Ref4.self, numberOfTimes: N)
}

// method.2x.
public func run_method2xVal0(_ N: Int) {
  run_method2x(withType: Val0.self, numberOfTimes: N)
}
public func run_method2xVal1(_ N: Int) {
  run_method2x(withType: Val1.self, numberOfTimes: N)
}
public func run_method2xVal2(_ N: Int) {
  run_method2x(withType: Val2.self, numberOfTimes: N)
}
public func run_method2xVal3(_ N: Int) {
  run_method2x(withType: Val3.self, numberOfTimes: N)
}
public func run_method2xVal4(_ N: Int) {
  run_method2x(withType: Val4.self, numberOfTimes: N)
}
public func run_method2xRef1(_ N: Int) {
  run_method2x(withType: Ref1.self, numberOfTimes: N)
}
public func run_method2xRef2(_ N: Int) {
  run_method2x(withType: Ref2.self, numberOfTimes: N)
}
public func run_method2xRef3(_ N: Int) {
  run_method2x(withType: Ref3.self, numberOfTimes: N)
}
public func run_method2xRef4(_ N: Int) {
  run_method2x(withType: Ref4.self, numberOfTimes: N)
}

// Pass.method.1x.
public func run_Pass_method1xVal0(_ N: Int) {
  run_Pass_method1x(withType: Val0.self, numberOfTimes: N)
}
public func run_Pass_method1xVal1(_ N: Int) {
  run_Pass_method1x(withType: Val1.self, numberOfTimes: N)
}
public func run_Pass_method1xVal2(_ N: Int) {
  run_Pass_method1x(withType: Val2.self, numberOfTimes: N)
}
public func run_Pass_method1xVal3(_ N: Int) {
  run_Pass_method1x(withType: Val3.self, numberOfTimes: N)
}
public func run_Pass_method1xVal4(_ N: Int) {
  run_Pass_method1x(withType: Val4.self, numberOfTimes: N)
}
public func run_Pass_method1xRef1(_ N: Int) {
  run_Pass_method1x(withType: Ref1.self, numberOfTimes: N)
}
public func run_Pass_method1xRef2(_ N: Int) {
  run_Pass_method1x(withType: Ref2.self, numberOfTimes: N)
}
public func run_Pass_method1xRef3(_ N: Int) {
  run_Pass_method1x(withType: Ref3.self, numberOfTimes: N)
}
public func run_Pass_method1xRef4(_ N: Int) {
  run_Pass_method1x(withType: Ref4.self, numberOfTimes: N)
}

// Pass.method.2x.
public func run_Pass_method2xVal0(_ N: Int) {
  run_Pass_method2x(withType: Val0.self, numberOfTimes: N)
}
public func run_Pass_method2xVal1(_ N: Int) {
  run_Pass_method2x(withType: Val1.self, numberOfTimes: N)
}
public func run_Pass_method2xVal2(_ N: Int) {
  run_Pass_method2x(withType: Val2.self, numberOfTimes: N)
}
public func run_Pass_method2xVal3(_ N: Int) {
  run_Pass_method2x(withType: Val3.self, numberOfTimes: N)
}
public func run_Pass_method2xVal4(_ N: Int) {
  run_Pass_method2x(withType: Val4.self, numberOfTimes: N)
}
public func run_Pass_method2xRef1(_ N: Int) {
  run_Pass_method2x(withType: Ref1.self, numberOfTimes: N)
}
public func run_Pass_method2xRef2(_ N: Int) {
  run_Pass_method2x(withType: Ref2.self, numberOfTimes: N)
}
public func run_Pass_method2xRef3(_ N: Int) {
  run_Pass_method2x(withType: Ref3.self, numberOfTimes: N)
}
public func run_Pass_method2xRef4(_ N: Int) {
  run_Pass_method2x(withType: Ref4.self, numberOfTimes: N)
}

// Mutating.
public func run_MutatingVal0(_ N: Int) {
  run_Mutating(withType: Val0.self, numberOfTimes: N)
}
public func run_MutatingVal1(_ N: Int) {
  run_Mutating(withType: Val1.self, numberOfTimes: N)
}
public func run_MutatingVal2(_ N: Int) {
  run_Mutating(withType: Val2.self, numberOfTimes: N)
}
public func run_MutatingVal3(_ N: Int) {
  run_Mutating(withType: Val3.self, numberOfTimes: N)
}
public func run_MutatingVal4(_ N: Int) {
  run_Mutating(withType: Val4.self, numberOfTimes: N)
}
public func run_MutatingRef1(_ N: Int) {
  run_Mutating(withType: Ref1.self, numberOfTimes: N)
}
public func run_MutatingRef2(_ N: Int) {
  run_Mutating(withType: Ref2.self, numberOfTimes: N)
}
public func run_MutatingRef3(_ N: Int) {
  run_Mutating(withType: Ref3.self, numberOfTimes: N)
}
public func run_MutatingRef4(_ N: Int) {
  run_Mutating(withType: Ref4.self, numberOfTimes: N)
}

// MutatingAndNonMutating.
public func run_MutatingAndNonMutatingVal0(_ N: Int) {
  run_MutatingAndNonMutating(withType: Val0.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingVal1(_ N: Int) {
  run_MutatingAndNonMutating(withType: Val1.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingVal2(_ N: Int) {
  run_MutatingAndNonMutating(withType: Val2.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingVal3(_ N: Int) {
  run_MutatingAndNonMutating(withType: Val3.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingVal4(_ N: Int) {
  run_MutatingAndNonMutating(withType: Val4.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingRef1(_ N: Int) {
  run_MutatingAndNonMutating(withType: Ref1.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingRef2(_ N: Int) {
  run_MutatingAndNonMutating(withType: Ref2.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingRef3(_ N: Int) {
  run_MutatingAndNonMutating(withType: Ref3.self, numberOfTimes: N)
}
public func run_MutatingAndNonMutatingRef4(_ N: Int) {
  run_MutatingAndNonMutating(withType: Ref4.self, numberOfTimes: N)
}

// Array.init.
public func run_Array_initVal0(_ N: Int) {
  run_Array_init(withType: Val0.self, numberOfTimes: N)
}
public func run_Array_initVal1(_ N: Int) {
  run_Array_init(withType: Val1.self, numberOfTimes: N)
}
public func run_Array_initVal2(_ N: Int) {
  run_Array_init(withType: Val2.self, numberOfTimes: N)
}
public func run_Array_initVal3(_ N: Int) {
  run_Array_init(withType: Val3.self, numberOfTimes: N)
}
public func run_Array_initVal4(_ N: Int) {
  run_Array_init(withType: Val4.self, numberOfTimes: N)
}
public func run_Array_initRef1(_ N: Int) {
  run_Array_init(withType: Ref1.self, numberOfTimes: N)
}
public func run_Array_initRef2(_ N: Int) {
  run_Array_init(withType: Ref2.self, numberOfTimes: N)
}
public func run_Array_initRef3(_ N: Int) {
  run_Array_init(withType: Ref3.self, numberOfTimes: N)
}
public func run_Array_initRef4(_ N: Int) {
  run_Array_init(withType: Ref4.self, numberOfTimes: N)
}

// Array.method.1x.
public func run_Array_method1xVal0(_ N: Int) {
  run_Array_method1x(withType: Val0.self, numberOfTimes: N)
}
public func run_Array_method1xVal1(_ N: Int) {
  run_Array_method1x(withType: Val1.self, numberOfTimes: N)
}
public func run_Array_method1xVal2(_ N: Int) {
  run_Array_method1x(withType: Val2.self, numberOfTimes: N)
}
public func run_Array_method1xVal3(_ N: Int) {
  run_Array_method1x(withType: Val3.self, numberOfTimes: N)
}
public func run_Array_method1xVal4(_ N: Int) {
  run_Array_method1x(withType: Val4.self, numberOfTimes: N)
}
public func run_Array_method1xRef1(_ N: Int) {
  run_Array_method1x(withType: Ref1.self, numberOfTimes: N)
}
public func run_Array_method1xRef2(_ N: Int) {
  run_Array_method1x(withType: Ref2.self, numberOfTimes: N)
}
public func run_Array_method1xRef3(_ N: Int) {
  run_Array_method1x(withType: Ref3.self, numberOfTimes: N)
}
public func run_Array_method1xRef4(_ N: Int) {
  run_Array_method1x(withType: Ref4.self, numberOfTimes: N)
}

// Array.method.2x.
public func run_Array_method2xVal0(_ N: Int) {
  run_Array_method2x(withType: Val0.self, numberOfTimes: N)
}
public func run_Array_method2xVal1(_ N: Int) {
  run_Array_method2x(withType: Val1.self, numberOfTimes: N)
}
public func run_Array_method2xVal2(_ N: Int) {
  run_Array_method2x(withType: Val2.self, numberOfTimes: N)
}
public func run_Array_method2xVal3(_ N: Int) {
  run_Array_method2x(withType: Val3.self, numberOfTimes: N)
}
public func run_Array_method2xVal4(_ N: Int) {
  run_Array_method2x(withType: Val4.self, numberOfTimes: N)
}
public func run_Array_method2xRef1(_ N: Int) {
  run_Array_method2x(withType: Ref1.self, numberOfTimes: N)
}
public func run_Array_method2xRef2(_ N: Int) {
  run_Array_method2x(withType: Ref2.self, numberOfTimes: N)
}
public func run_Array_method2xRef3(_ N: Int) {
  run_Array_method2x(withType: Ref3.self, numberOfTimes: N)
}
public func run_Array_method2xRef4(_ N: Int) {
  run_Array_method2x(withType: Ref4.self, numberOfTimes: N)
}

// Array.Mutating.
public func run_ArrayMutatingVal0(_ N: Int) {
  run_ArrayMutating(withType: Val0.self, numberOfTimes: N)
}
public func run_ArrayMutatingVal1(_ N: Int) {
  run_ArrayMutating(withType: Val1.self, numberOfTimes: N)
}
public func run_ArrayMutatingVal2(_ N: Int) {
  run_ArrayMutating(withType: Val2.self, numberOfTimes: N)
}
public func run_ArrayMutatingVal3(_ N: Int) {
  run_ArrayMutating(withType: Val3.self, numberOfTimes: N)
}
public func run_ArrayMutatingVal4(_ N: Int) {
  run_ArrayMutating(withType: Val4.self, numberOfTimes: N)
}
public func run_ArrayMutatingRef1(_ N: Int) {
  run_ArrayMutating(withType: Ref1.self, numberOfTimes: N)
}
public func run_ArrayMutatingRef2(_ N: Int) {
  run_ArrayMutating(withType: Ref2.self, numberOfTimes: N)
}
public func run_ArrayMutatingRef3(_ N: Int) {
  run_ArrayMutating(withType: Ref3.self, numberOfTimes: N)
}
public func run_ArrayMutatingRef4(_ N: Int) {
  run_ArrayMutating(withType: Ref4.self, numberOfTimes: N)
}

// Array.Shift.
public func run_ArrayShiftVal0(_ N: Int) {
  run_ArrayShift(withType: Val0.self, numberOfTimes: N)
}
public func run_ArrayShiftVal1(_ N: Int) {
  run_ArrayShift(withType: Val1.self, numberOfTimes: N)
}
public func run_ArrayShiftVal2(_ N: Int) {
  run_ArrayShift(withType: Val2.self, numberOfTimes: N)
}
public func run_ArrayShiftVal3(_ N: Int) {
  run_ArrayShift(withType: Val3.self, numberOfTimes: N)
}
public func run_ArrayShiftVal4(_ N: Int) {
  run_ArrayShift(withType: Val4.self, numberOfTimes: N)
}
public func run_ArrayShiftRef1(_ N: Int) {
  run_ArrayShift(withType: Ref1.self, numberOfTimes: N)
}
public func run_ArrayShiftRef2(_ N: Int) {
  run_ArrayShift(withType: Ref2.self, numberOfTimes: N)
}
public func run_ArrayShiftRef3(_ N: Int) {
  run_ArrayShift(withType: Ref3.self, numberOfTimes: N)
}
public func run_ArrayShiftRef4(_ N: Int) {
  run_ArrayShift(withType: Ref4.self, numberOfTimes: N)
}

// Array.ConditionalShift.
public func run_ArrayConditionalShiftVal0(_ N: Int) {
  run_ArrayConditionalShift(withType: Val0.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftVal1(_ N: Int) {
  run_ArrayConditionalShift(withType: Val1.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftVal2(_ N: Int) {
  run_ArrayConditionalShift(withType: Val2.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftVal3(_ N: Int) {
  run_ArrayConditionalShift(withType: Val3.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftVal4(_ N: Int) {
  run_ArrayConditionalShift(withType: Val4.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftRef1(_ N: Int) {
  run_ArrayConditionalShift(withType: Ref1.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftRef2(_ N: Int) {
  run_ArrayConditionalShift(withType: Ref2.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftRef3(_ N: Int) {
  run_ArrayConditionalShift(withType: Ref3.self, numberOfTimes: N)
}
public func run_ArrayConditionalShiftRef4(_ N: Int) {
  run_ArrayConditionalShift(withType: Ref4.self, numberOfTimes: N)
}

// Local Variables:
// eval: (read-only-mode 1)
// End:

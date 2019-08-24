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

// The purpose of these benchmarks is to evaluate different scenarios when
// moving the implementation of existentials (protocol values) to heap based
// copy-on-write buffers.
//
// The performance boost of `Ref4` vs `Ref3` is expected because copying the
// existential only involves copying one reference of the heap based
// copy-on-write buffer (outline case) that holds the struct vs copying the
// individual fields of the struct in the inline case of `Ref3`.

let t: [BenchmarkCategory] = [.skip]
let ta: [BenchmarkCategory] = [.api, .Array, .skip]

public let ExistentialPerformance: [BenchmarkInfo] = [
  BenchmarkInfo(name: "Existential.method.1x.Ref1",
    runFunction: run_method1x, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.method.1x.Ref2",
    runFunction: run_method1x, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.method.1x.Ref3",
    runFunction: run_method1x, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.method.1x.Ref4",
    runFunction: run_method1x, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.method.1x.Val0",
    runFunction: run_method1x, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.method.1x.Val1",
    runFunction: run_method1x, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.method.1x.Val2",
    runFunction: run_method1x, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.method.1x.Val3",
    runFunction: run_method1x, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.method.1x.Val4",
    runFunction: run_method1x, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.method.2x.Ref1",
    runFunction: run_method2x, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.method.2x.Ref2",
    runFunction: run_method2x, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.method.2x.Ref3",
    runFunction: run_method2x, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.method.2x.Ref4",
    runFunction: run_method2x, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.method.2x.Val0",
    runFunction: run_method2x, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.method.2x.Val1",
    runFunction: run_method2x, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.method.2x.Val2",
    runFunction: run_method2x, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.method.2x.Val3",
    runFunction: run_method2x, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.method.2x.Val4",
    runFunction: run_method2x, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref1",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref2",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref3",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Ref4",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val0",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val1",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val2",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val3",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.Pass.method.1x.Val4",
    runFunction: run_Pass_method1x, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref1",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref2",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref3",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Ref4",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val0",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val1",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val2",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val3",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.Pass.method.2x.Val4",
    runFunction: run_Pass_method2x, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.Mutating.Ref1",
    runFunction: run_Mutating, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.Mutating.Ref2",
    runFunction: run_Mutating, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.Mutating.Ref3",
    runFunction: run_Mutating, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.Mutating.Ref4",
    runFunction: run_Mutating, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.Mutating.Val0",
    runFunction: run_Mutating, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.Mutating.Val1",
    runFunction: run_Mutating, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.Mutating.Val2",
    runFunction: run_Mutating, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.Mutating.Val3",
    runFunction: run_Mutating, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.Mutating.Val4",
    runFunction: run_Mutating, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref1",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref2",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref3",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Ref4",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val0",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val1",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val2",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val3",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.MutatingAndNonMutating.Val4",
    runFunction: run_MutatingAndNonMutating, tags: t, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.Array.init.Ref1",
    runFunction: run_Array_init, tags: ta, setUpFunction: etRef1),
  BenchmarkInfo(name: "Existential.Array.init.Ref2",
    runFunction: run_Array_init, tags: ta, setUpFunction: etRef2),
  BenchmarkInfo(name: "Existential.Array.init.Ref3",
    runFunction: run_Array_init, tags: ta, setUpFunction: etRef3),
  BenchmarkInfo(name: "Existential.Array.init.Ref4",
    runFunction: run_Array_init, tags: ta, setUpFunction: etRef4),
  BenchmarkInfo(name: "Existential.Array.init.Val0",
    runFunction: run_Array_init, tags: ta, setUpFunction: etVal0),
  BenchmarkInfo(name: "Existential.Array.init.Val1",
    runFunction: run_Array_init, tags: ta, setUpFunction: etVal1),
  BenchmarkInfo(name: "Existential.Array.init.Val2",
    runFunction: run_Array_init, tags: ta, setUpFunction: etVal2),
  BenchmarkInfo(name: "Existential.Array.init.Val3",
    runFunction: run_Array_init, tags: ta, setUpFunction: etVal3),
  BenchmarkInfo(name: "Existential.Array.init.Val4",
    runFunction: run_Array_init, tags: ta, setUpFunction: etVal4),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref1",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref2",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref3",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.method.1x.Ref4",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val0",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val1",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val2",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val3",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.method.1x.Val4",
    runFunction: run_Array_method1x, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref1",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref2",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref3",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.method.2x.Ref4",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val0",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val1",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val2",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val3",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.method.2x.Val4",
    runFunction: run_Array_method2x, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref1",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref2",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref3",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.Mutating.Ref4",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val0",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val1",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val2",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val3",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.Mutating.Val4",
    runFunction: run_ArrayMutating, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref1",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref2",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref3",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.Shift.Ref4",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.Shift.Val0",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.Shift.Val1",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.Shift.Val2",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.Shift.Val3",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.Shift.Val4",
    runFunction: run_ArrayShift, tags: ta, setUpFunction: caVal4),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref1",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caRef1),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref2",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caRef2),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref3",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caRef3),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Ref4",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caRef4),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val0",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caVal0),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val1",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caVal1),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val2",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caVal2),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val3",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caVal3),
  BenchmarkInfo(name: "Existential.Array.ConditionalShift.Val4",
    runFunction: run_ArrayConditionalShift, tags: ta, setUpFunction: caVal4),
]

// To exclude the setup overhead of existential array initialization,
// these are setup functions that **create array** for each variant type.
var array: [Existential]!
func ca<T: Existential>(_: T.Type) {
  array = Array(repeating: T(), count: 128)
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

// `setUpFunctions` that determine which existential type will be tested
var existentialType: Existential.Type!
func etVal0() { existentialType = Val0.self }
func etVal1() { existentialType = Val1.self }
func etVal2() { existentialType = Val2.self }
func etVal3() { existentialType = Val3.self }
func etVal4() { existentialType = Val4.self }
func etRef1() { existentialType = Ref1.self }
func etRef2() { existentialType = Ref2.self }
func etRef3() { existentialType = Ref3.self }
func etRef4() { existentialType = Ref4.self }

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
func passExistentialTwiceOneMethodCall(_ e0: Existential, _ e1: Existential)
  -> Bool {
  return e0.doIt() && e1.doIt()
}

@inline(never)
func passExistentialTwiceTwoMethodCalls(_ e0: Existential, _ e1: Existential)
  -> Bool {
  return e0.doIt() && e1.doIt() && e0.reallyDoIt() && e1.reallyDoIt()
}

func run_method1x(_ N: Int) {
  let existential = existentialType.init()
  for _ in 0 ..< N * 20_000 {
    if !existential.doIt() {
      fatalError("expected true")
    }
  }
}

func run_method2x(_ N: Int) {
  let existential = existentialType.init()
  for _ in 0 ..< N * 20_000 {
    if !existential.doIt()  || !existential.reallyDoIt() {
      fatalError("expected true")
    }
  }
}

func run_Pass_method1x(_ N: Int) {
  let existential = existentialType.init()
  let existential2 = existentialType.init()
  for _ in 0 ..< N * 20_000 {
    if !passExistentialTwiceOneMethodCall(existential, existential2) {
      fatalError("expected true")
    }
  }
}

func run_Pass_method2x(_ N: Int) {
  let existential = existentialType.init()
  let existential2 = existentialType.init()
  for _ in 0 ..< N * 20_000 {
    if !passExistentialTwiceTwoMethodCalls(existential, existential2) {
      fatalError("expected true")
    }
  }
}

func run_Mutating(_ N: Int) {
  var existential = existentialType.init()
  for _ in 0 ..< N * 10_000 {
    if !existential.mutateIt()  {
      fatalError("expected true")
    }
  }
}

func run_MutatingAndNonMutating(_ N: Int) {
  var existential = existentialType.init()
  for _ in 0 ..< N * 10_000 {
    let _ = existential.doIt()
    if !existential.mutateIt()  {
      fatalError("expected true")
    }
  }
}

func run_Array_init(_ N: Int) {

  for _ in 0 ..< N * 100 {
    blackHole(Array(repeating: existentialType.init(), count: 128))
  }
}

func run_Array_method1x(_ N: Int) {
  let existentialArray = array!
  for _ in 0 ..< N * 100 {
    for elt in existentialArray {
      if !elt.doIt()  {
        fatalError("expected true")
      }
    }
  }
}

func run_Array_method2x(_ N: Int) {
  let existentialArray = array!
  for _ in 0 ..< N * 100 {
    for elt in existentialArray {
      if !elt.doIt() || !elt.reallyDoIt() {
        fatalError("expected true")
      }
    }
  }
}

func run_ArrayMutating(_ N: Int) {
  var existentialArray = array!
  for _ in 0 ..< N * 500 {
    for i in 0 ..< existentialArray.count {
      if !existentialArray[i].mutateIt()  {
        fatalError("expected true")
      }
    }
  }
}

func run_ArrayShift(_ N: Int) {
  var existentialArray = array!
  for _ in 0 ..< N * 25 {
    for i in 0 ..< existentialArray.count-1 {
      existentialArray.swapAt(i, i+1)
    }
  }
}

func run_ArrayConditionalShift(_ N: Int) {
  var existentialArray = array!
  for _ in 0 ..< N * 25 {
    for i in 0 ..< existentialArray.count-1 {
      let curr = existentialArray[i]
      if curr.doIt() {
        existentialArray[i] = existentialArray[i+1]
        existentialArray[i+1] = curr
      }
    }
  }
}

// Local Variables:
// eval: (read-only-mode 1)
// End:

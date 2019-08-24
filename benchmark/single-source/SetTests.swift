//===--- SetTests.swift ---------------------------------------------------===//
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

import TestsUtils

let size = 400
let half = size / 2
let quarter = size / 4

// Construction of empty sets.
let setE: Set<Int> = []
let setOE: Set<Box<Int>> = []

// Construction kit for sets with 25% overlap
let setAB = Set(0 ..< size)                              //   0 ..< 400
let setCD = Set(size ..< 2 * size)                       // 400 ..< 800
let setBC = Set(size - quarter ..< 2 * size - quarter)   // 300 ..< 700
let setB = Set(size - quarter ..< size)                  // 300 ..< 400

let setOAB = Set(setAB.map(Box.init))
let setOCD = Set(setCD.map(Box.init))
let setOBC = Set(setBC.map(Box.init))
let setOB = Set(setB.map(Box.init))

let countA = size - quarter        // 300
let countB = quarter               // 100
let countC = countA                // 300
let countD = countB                // 100

let countAB = size                 // 400
let countAC = countA + countC      // 600
let countABC = countA + countB + countC           // 700
let countABCD = countA + countB + countC + countD // 800

// Construction kit for sets with 50% overlap
let setXY = Set(0 ..< size)           //   0 ..< 400
let setYZ = Set(half ..< size + half) // 200 ..< 600
let setY = Set(half ..< size)         // 200 ..< 400

// Two sets with 100% overlap, but different bucket counts (let's not make it
// too easy...)
let setP = Set(0 ..< size)
let setQ: Set<Int> = {
  var set = Set(0 ..< size)
  set.reserveCapacity(2 * size)
  return set
}()

// Construction of empty array.
let arrayE: Array<Int> = []
let arrayOE: Array<Box<Int>> = []

// Construction kit for arrays with 25% overlap
let arrayAB = Array(0 ..< size)                              //   0 ..< 400
let arrayCD = Array(size ..< 2 * size)                       // 400 ..< 800
let arrayBC = Array(size - quarter ..< 2 * size - quarter)   // 300 ..< 700
let arrayB = Array(size - quarter ..< size)                  // 300 ..< 400

let arrayOAB = arrayAB.map(Box.init)
let arrayOCD = arrayCD.map(Box.init)
let arrayOBC = arrayBC.map(Box.init)
let arrayOB = arrayB.map(Box.init)

// Construction kit for arrays with 50% overlap
let arrayXY = Array(0 ..< size)           //   0 ..< 400
let arrayYZ = Array(half ..< size + half) // 200 ..< 600
let arrayY = Array(half ..< size)         // 200 ..< 400

let arrayP = Array(0 ..< size)

// Construction of flexible sets.
var set: Set<Int> = []
var setBox: Set<Box<Int>> = []

func set(_ size: Int) {
  set = Set(0 ..< size)
}

func setBox(_ size: Int) {
  setBox = Set(Set(0 ..< size).map(Box.init))
}


public let SetTests = [
  // Mnemonic: number after name is percentage of common elements in input sets.
  BenchmarkInfo(
    name: "Set.isSubset.Empty.Int",
    runFunction: { n in run_SetIsSubsetInt(setE, setAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, setAB]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Int.Empty",
    runFunction: { n in run_SetIsSubsetInt(setAB, setE, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setE]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt0",
    runFunction: { n in run_SetIsSubsetInt(setAB, setCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetBox0",
    runFunction: { n in run_SetIsSubsetBox(setOAB, setOCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt25",
    runFunction: { n in run_SetIsSubsetInt(setB, setAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, setAB]) }),
  BenchmarkInfo(
    name: "SetIsSubsetBox25",
    runFunction: { n in run_SetIsSubsetBox(setOB, setOAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, setOAB]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt50",
    runFunction: { n in run_SetIsSubsetInt(setY, setXY, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, setXY]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt100",
    runFunction: { n in run_SetIsSubsetInt(setP, setQ, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.isSubset.Seq.Empty.Int",
    runFunction: { n in run_SetIsSubsetSeqInt(setE, arrayAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Int.Empty",
    runFunction: { n in run_SetIsSubsetSeqInt(setAB, arrayE, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Int0",
    runFunction: { n in run_SetIsSubsetSeqInt(setAB, arrayCD, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayCD]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Box0",
    runFunction: { n in run_SetIsSubsetSeqBox(setOAB, arrayOCD, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOCD]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Int25",
    runFunction: { n in run_SetIsSubsetSeqInt(setB, arrayBC, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Box25",
    runFunction: { n in run_SetIsSubsetSeqBox(setOB, arrayOBC, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Int50",
    runFunction: { n in run_SetIsSubsetSeqInt(setY, arrayYZ, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.isSubset.Seq.Int100",
    runFunction: { n in run_SetIsSubsetSeqInt(setP, arrayP, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "Set.isStrictSubset.Empty.Int",
    runFunction: { n in run_SetIsStrictSubsetInt(setE, setAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, setAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Int.Empty",
    runFunction: { n in run_SetIsStrictSubsetInt(setAB, setE, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setE]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Int0",
    runFunction: { n in run_SetIsStrictSubsetInt(setAB, setCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Box0",
    runFunction: { n in run_SetIsStrictSubsetBox(setOAB, setOCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Int25",
    runFunction: { n in run_SetIsStrictSubsetInt(setB, setAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, setAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Box25",
    runFunction: { n in run_SetIsStrictSubsetBox(setOB, setOAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, setOAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Int50",
    runFunction: { n in run_SetIsStrictSubsetInt(setY, setXY, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, setXY]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Int100",
    runFunction: { n in run_SetIsStrictSubsetInt(setP, setQ, false, 5000 * n) },
    tags: [.validation, .api, .Set, .skip],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Empty.Int",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setE, arrayAB, true, 5000 * n) },
    tags: [.validation, .api, .Set, .skip],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Int.Empty",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setAB, arrayE, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Int0",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setAB, arrayCD, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayCD]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Box0",
    runFunction: { n in run_SetIsStrictSubsetSeqBox(setOAB, arrayOCD, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOCD]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Int25",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setB, arrayBC, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Box25",
    runFunction: { n in run_SetIsStrictSubsetSeqBox(setOB, arrayOBC, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Int50",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setY, arrayYZ, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.isStrictSubset.Seq.Int100",
    runFunction: { n in run_SetIsStrictSubsetSeqInt(setP, arrayP, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Empty.Int",
    runFunction: { n in run_SetIsSupersetSeqInt(setAB, arrayE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Int.Empty",
    runFunction: { n in run_SetIsSupersetSeqInt(setE, arrayAB, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Int0",
    runFunction: { n in run_SetIsSupersetSeqInt(setCD, arrayAB, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setCD, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Box0",
    runFunction: { n in run_SetIsSupersetSeqBox(setOCD, arrayOAB, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOCD, arrayOAB]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Int25",
    runFunction: { n in run_SetIsSupersetSeqInt(setB, arrayBC, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Box25",
    runFunction: { n in run_SetIsSupersetSeqBox(setOB, arrayOBC, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Int50",
    runFunction: { n in run_SetIsSupersetSeqInt(setY, arrayYZ, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.isSuperset.Seq.Int100",
    runFunction: { n in run_SetIsSupersetSeqInt(setP, arrayP, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Empty.Int",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setAB, arrayE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Int.Empty",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setE, arrayAB, false, 5000 * n) },
    tags: [.validation, .api, .Set, .skip],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Int0",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setCD, arrayAB, false, 500 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setCD, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Box0",
    runFunction: { n in run_SetIsStrictSupersetSeqBox(setOCD, arrayOAB, false, 500 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOCD, arrayOAB]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Int25",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setB, arrayBC, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Box25",
    runFunction: { n in run_SetIsStrictSupersetSeqBox(setOB, arrayOBC, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Int50",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setY, arrayYZ, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.isStrictSuperset.Seq.Int100",
    runFunction: { n in run_SetIsStrictSupersetSeqInt(setP, arrayP, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "Set.isDisjoint.Empty.Int",
    runFunction: { n in run_SetIsDisjointInt(setE, setAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, setAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Int.Empty",
    runFunction: { n in run_SetIsDisjointInt(setAB, setE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setE]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Empty.Box",
    runFunction: { n in run_SetIsDisjointBox(setOE, setOAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOE, setOAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Box.Empty",
    runFunction: { n in run_SetIsDisjointBox(setOAB, setOE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOE]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Int0",
    runFunction: { n in run_SetIsDisjointInt(setAB, setCD, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Box0",
    runFunction: { n in run_SetIsDisjointBox(setOAB, setOCD, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Int25",
    runFunction: { n in run_SetIsDisjointInt(setB, setAB, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, setAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Box25",
    runFunction: { n in run_SetIsDisjointBox(setOB, setOAB, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, setOAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Int50",
    runFunction: { n in run_SetIsDisjointInt(setY, setXY, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, setXY]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Int100",
    runFunction: { n in run_SetIsDisjointInt(setP, setQ, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Empty.Int",
    runFunction: { n in run_SetIsDisjointSeqInt(setE, arrayAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Int.Empty",
    runFunction: { n in run_SetIsDisjointSeqInt(setAB, arrayE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Empty.Box",
    runFunction: { n in run_SetIsDisjointSeqBox(setOE, arrayOAB, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOE, arrayOAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Box.Empty",
    runFunction: { n in run_SetIsDisjointSeqBox(setOAB, arrayOE, true, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOE]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Int0",
    runFunction: { n in run_SetIsDisjointSeqInt(setAB, arrayCD, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayCD]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Box0",
    runFunction: { n in run_SetIsDisjointSeqBox(setOAB, arrayOCD, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOCD]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Int25",
    runFunction: { n in run_SetIsDisjointSeqInt(setB, arrayAB, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Box25",
    runFunction: { n in run_SetIsDisjointSeqBox(setOB, arrayOAB, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, arrayOAB]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Int50",
    runFunction: { n in run_SetIsDisjointSeqInt(setY, arrayXY, false, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, arrayXY]) }),
  BenchmarkInfo(
    name: "Set.isDisjoint.Seq.Int100",
    runFunction: { n in run_SetIsDisjointSeqInt(setP, arrayP, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt0",
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceBox0",
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt25",
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceBox25",
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt50",
    runFunction: { n in run_SetSymmetricDifferenceInt(setXY, setYZ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt100",
    runFunction: { n in run_SetSymmetricDifferenceInt(setP, setQ, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "SetIntersectionInt0",
    runFunction: { n in run_SetIntersectionInt(setAB, setCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIntersectionBox0",
    runFunction: { n in run_SetIntersectionBox(setOAB, setOCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt25",
    runFunction: { n in run_SetIntersectionInt(setAB, setBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetIntersectionBox25",
    runFunction: { n in run_SetIntersectionBox(setOAB, setOBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt50",
    runFunction: { n in run_SetIntersectionInt(setXY, setYZ, half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt100",
    runFunction: { n in run_SetIntersectionInt(setP, setQ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.intersection.Seq.Int0",
    runFunction: { n in run_SetIntersectionSeqInt(setAB, arrayCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayCD]) }),
  BenchmarkInfo(
    name: "Set.intersection.Seq.Box0",
    runFunction: { n in run_SetIntersectionSeqBox(setOAB, arrayOCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOCD]) }),
  BenchmarkInfo(
    name: "Set.intersection.Seq.Int25",
    runFunction: { n in run_SetIntersectionSeqInt(setAB, arrayBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.intersection.Seq.Box25",
    runFunction: { n in run_SetIntersectionSeqBox(setOAB, arrayOBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.intersection.Seq.Int50",
    runFunction: { n in run_SetIntersectionSeqInt(setXY, arrayYZ, half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.intersection.Seq.Int100",
    runFunction: { n in run_SetIntersectionSeqInt(setP, arrayP, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

  BenchmarkInfo(
    name: "SetUnionInt0",
    runFunction: { n in run_SetUnionInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetUnionBox0",
    runFunction: { n in run_SetUnionBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetUnionInt25",
    runFunction: { n in run_SetUnionInt(setAB, setBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetUnionBox25",
    runFunction: { n in run_SetUnionBox(setOAB, setOBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetUnionInt50",
    runFunction: { n in run_SetUnionInt(setXY, setYZ, size + half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetUnionInt100",
    runFunction: { n in run_SetUnionInt(setP, setQ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.subtracting.Empty.Int",
    runFunction: { n in run_SetSubtractingInt(setE, setAB, 0, 1000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, setAB]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Int.Empty",
    runFunction: { n in run_SetSubtractingInt(setAB, setE, countAB, 1000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setE]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Empty.Box",
    runFunction: { n in run_SetSubtractingBox(setOE, setOAB, 0, 1000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOE, setOAB]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Box.Empty",
    runFunction: { n in run_SetSubtractingBox(setOAB, setOE, countAB, 1000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOE]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt0",
    runFunction: { n in run_SetSubtractingInt(setAB, setCD, countAB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetSubtractingBox0",
    runFunction: { n in run_SetSubtractingBox(setOAB, setOCD, countAB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt25",
    runFunction: { n in run_SetSubtractingInt(setAB, setBC, countA, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetSubtractingBox25",
    runFunction: { n in run_SetSubtractingBox(setOAB, setOBC, countA, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt50",
    runFunction: { n in run_SetSubtractingInt(setXY, setYZ, half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt100",
    runFunction: { n in run_SetSubtractingInt(setP, setQ, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "Set.subtracting.Seq.Empty.Int",
    runFunction: { n in run_SetSubtractingSeqInt(setE, arrayAB, 0, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setE, arrayAB]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Int.Empty",
    runFunction: { n in run_SetSubtractingSeqInt(setAB, arrayE, countAB, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayE]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Empty.Box",
    runFunction: { n in run_SetSubtractingSeqBox(setOE, arrayOAB, 0, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOE, arrayOAB]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Box.Empty",
    runFunction: { n in run_SetSubtractingSeqBox(setOAB, arrayOE, countAB, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOE]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Int0",
    runFunction: { n in run_SetSubtractingSeqInt(setAB, arrayCD, countAB, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayCD]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Box0",
    runFunction: { n in run_SetSubtractingSeqBox(setOAB, arrayOCD, countAB, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOCD]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Int25",
    runFunction: { n in run_SetSubtractingSeqInt(setAB, arrayBC, countA, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, arrayBC]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Box25",
    runFunction: { n in run_SetSubtractingSeqBox(setOAB, arrayOBC, countA, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, arrayOBC]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Int50",
    runFunction: { n in run_SetSubtractingSeqInt(setXY, arrayYZ, half, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, arrayYZ]) }),
  BenchmarkInfo(
    name: "Set.subtracting.Seq.Int100",
    runFunction: { n in run_SetSubtractingSeqInt(setP, arrayP, 0, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, arrayP]) }),

    BenchmarkInfo(
    name: "Set.filter.Int50.16k",
    runFunction: { n in run_SetFilterInt50(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(16_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int50.20k",
    runFunction: { n in run_SetFilterInt50(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(20_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int50.24k",
    runFunction: { n in run_SetFilterInt50(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(24_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int50.28k",
    runFunction: { n in run_SetFilterInt50(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(28_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int100.16k",
    runFunction: { n in run_SetFilterInt100(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(16_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int100.20k",
    runFunction: { n in run_SetFilterInt100(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(20_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int100.24k",
    runFunction: { n in run_SetFilterInt100(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(24_000) }),
  BenchmarkInfo(
    name: "Set.filter.Int100.28k",
    runFunction: { n in run_SetFilterInt100(n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { set(28_000) }),

  // Legacy benchmarks, kept for continuity with previous releases.
  BenchmarkInfo(
    name: "SetExclusiveOr", // ~"SetSymmetricDifferenceInt0"
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }, legacyFactor: 10),
  BenchmarkInfo(
    name: "SetExclusiveOr_OfObjects", // ~"SetSymmetricDifferenceBox0"
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }, legacyFactor: 10),
  BenchmarkInfo(
    name: "SetIntersect", // ~"SetIntersectionInt0"
    runFunction: { n in run_SetIntersectionInt(setAB, setCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }, legacyFactor: 10),
  BenchmarkInfo(
    name: "SetUnion", // ~"SetUnionInt0"
    runFunction: { n in run_SetUnionInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }, legacyFactor: 10),
  BenchmarkInfo(
    name: "SetUnion_OfObjects", // ~"SetUnionBox0"
    runFunction: { n in run_SetUnionBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }, legacyFactor: 10),
]

@inline(never)
public func run_SetIsSubsetInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
public func run_SetIsSubsetSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
public func run_SetIsStrictSubsetInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSubset = a.isStrictSubset(of: identity(b))
    CheckResults(isStrictSubset == r)
  }
}

@inline(never)
public func run_SetIsStrictSubsetSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSubset = a.isStrictSubset(of: identity(b))
    CheckResults(isStrictSubset == r)
  }
}

@inline(never)
public func run_SetIsSupersetSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSuperset = a.isSuperset(of: identity(b))
    CheckResults(isSuperset == r)
  }
}

@inline(never)
public func run_SetIsStrictSupersetSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSuperset = a.isStrictSuperset(of: identity(b))
    CheckResults(isStrictSuperset == r)
  }
}

@inline(never)
public func run_SetSymmetricDifferenceInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let diff = a.symmetricDifference(identity(b))
    CheckResults(diff.count == r)
  }
}

@inline(never)
public func run_SetUnionInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let or = a.union(identity(b))
    CheckResults(or.count == r)
  }
}

@inline(never)
public func run_SetIntersectionInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
public func run_SetIntersectionSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
public func run_SetSubtractingInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
public func run_SetSubtractingSeqInt(
  _ a: Set<Int>,
  _ b: Array<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
public func run_SetIsDisjointInt(
    _ a: Set<Int>,
    _ b: Set<Int>,
    _ r: Bool,
    _ n: Int) {
    for _ in 0 ..< n {
        let isDisjoint = a.isDisjoint(with: identity(b))
        CheckResults(isDisjoint == r)
    }
}

@inline(never)
public func run_SetIsDisjointSeqInt(
    _ a: Set<Int>,
    _ b: Array<Int>,
    _ r: Bool,
    _ n: Int) {
    for _ in 0 ..< n {
        let isDisjoint = a.isDisjoint(with: identity(b))
        CheckResults(isDisjoint == r)
    }
}

@inline(never)
public func run_SetFilterInt50(_ n: Int) {
  for _ in 0 ..< n {
    let half = set.filter { $0 % 2 == 0 }
    CheckResults(set.count == half.count * 2)
  }
}

@inline(never)
public func run_SetFilterInt100(_ n: Int) {
  for _ in 0 ..< n {
    let copy = set.filter { _ in true }
    CheckResults(set.count == copy.count)
  }
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
func run_SetIsSubsetBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
func run_SetIsSubsetSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
func run_SetIsStrictSubsetBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSubset = a.isStrictSubset(of: identity(b))
    CheckResults(isStrictSubset == r)
  }
}

@inline(never)
func run_SetIsStrictSubsetSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSubset = a.isStrictSubset(of: identity(b))
    CheckResults(isStrictSubset == r)
  }
}

@inline(never)
func run_SetIsSupersetSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSuperset = a.isSuperset(of: identity(b))
    CheckResults(isSuperset == r)
  }
}

@inline(never)
func run_SetIsStrictSupersetSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isStrictSuperset = a.isStrictSuperset(of: identity(b))
    CheckResults(isStrictSuperset == r)
  }
}

@inline(never)
func run_SetSymmetricDifferenceBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let diff = a.symmetricDifference(identity(b))
    CheckResults(diff.count == r)
  }
}

@inline(never)
func run_SetUnionBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let or = a.union(identity(b))
    CheckResults(or.count == r)
  }
}

@inline(never)
func run_SetIntersectionBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(b)
    CheckResults(and.count == r)
  }
}

@inline(never)
func run_SetIntersectionSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
func run_SetSubtractingBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(b)
    CheckResults(and.count == r)
  }
}

@inline(never)
func run_SetSubtractingSeqBox(
  _ a: Set<Box<Int>>,
  _ b: Array<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
func run_SetIsDisjointBox(
    _ a: Set<Box<Int>>,
    _ b: Set<Box<Int>>,
    _ r: Bool,
    _ n: Int) {
    for _ in 0 ..< n {
        let isDisjoint = a.isDisjoint(with: identity(b))
        CheckResults(isDisjoint == r)
    }
}

@inline(never)
func run_SetIsDisjointSeqBox(
    _ a: Set<Box<Int>>,
    _ b: Array<Box<Int>>,
    _ r: Bool,
    _ n: Int) {
    for _ in 0 ..< n {
        let isDisjoint = a.isDisjoint(with: identity(b))
        CheckResults(isDisjoint == r)
    }
}

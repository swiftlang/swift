//===--- KeyPathPerformanceTests.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "KeyPathNestedStructs",
    runFunction: run_KeyPathNestedStructs,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
  BenchmarkInfo(
    name: "KeyPathDirectAccess",
    runFunction: run_testDirectAccess,
    tags: [.validation, .api, .skip],
    setUpFunction: setupSingleton
  ),
  BenchmarkInfo(
    name: "KeyPathReadPerformance",
    runFunction: run_testKeyPathReadPerformance,
    tags: [.validation, .api],
    setUpFunction: setupSingleton
  ),
  BenchmarkInfo(
    name: "KeyPathWritePerformance",
    runFunction: run_testKeyPathWritePerformance,
    tags: [.validation, .api],
    setUpFunction: setupSingleton
  ),
  BenchmarkInfo(
    name: "KeyPathsSmallStruct",
    runFunction: run_testKeyPathSmallStruct,
    tags: [.validation, .api],
    setUpFunction: setupSingleton
  ),
  BenchmarkInfo(
    name: "KeyPathMutatingGetset",
    runFunction: run_KeyPathMutatingGetset,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
  BenchmarkInfo(
    name: "KeyPathGet",
    runFunction: run_KeyPathGet,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
  BenchmarkInfo(
    name: "KeyPathOptionals",
    runFunction: run_KeyPathOptionals,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
  BenchmarkInfo(
    name: "KeyPathNestedClasses",
    runFunction: run_KeyPathNestedClasses,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
  BenchmarkInfo(
    name: "KeyPathClassStructs",
    runFunction: run_KeyPathClassStructs,
    tags: [.validation, .api],
    setUpFunction: setupKeyPathNestedStructs
  ),
]

/**
 There are several types of KeyPathComponents.
 The following table indicates which type is featured in which benchmark.
 Note that some types may be observed in other benchmarks as well.

 | KeyPathComponent Type |Featured in Benchmark             |
 |-----------------------|:---------------------------------|
 | struct                |*run_KeyPathNestedStruct()*       |
 | class                 |*run_KeyPathNestedClasses()*      |
 | get                   |*run_KeyPathGet()*                |
 | mutatingGetset        |*run_KeyPathMutatingGetset()*     |
 | nonmutatingGetset     |*run_KeyPathNonmutatingGetset()*  |
 | optionalChain         |*run_KeyPathOptionals()*          |
 | optionalForce         |*run_KeyPathOptionals()*          |
 | optionalWrap          |*run_KeyPathOptionals()*          |
 */

let numberOfElementsForNestedStructs = 20000
let expectedIntForNestedItems = 3

// Make it easy to switch between primitives of different types (Int, Float, Double, etc.).
typealias ElementType = Double

// The purpose of this class is to hold arrays used during the course of the benchmarks.
// This is to avoid storing them in the global scope, which can slow things down.
class FixedSizeArrayHolder {
  var fixedSizeArray100: FixedSizeArray100<ElementType>
  var fixedSizeArray10: FixedSizeArray10<ElementType>
  var mainArrayForNestedStructs: [A]
  var mainArrayForClassStructs: [D1]
  var arrayForMutatingGetset: [MutatingGetsetNested1]
  var arrayForGet: [GetNested1]
  var arrayForOptionals: [Optional1]
  var arrayForNestedClasses: [C1]
  var arrayForNonMutatingGetset: [M]
  var keypathForMutatingGetSet: WritableKeyPath<MutatingGetsetNested1, Int>
  var keypathForGet: KeyPath<GetNested1, Int>
  var keypathForOptional: KeyPath<Optional1, Int?>
  var keypathForNestedClasses: KeyPath<C1, Int>
  var keypathForNonMutatingGetset: WritableKeyPath<M, Int>
  var keypathForClassStructs: WritableKeyPath<D1, Int>

  // Same order as in KeyPathWritePerformance
  var kp46: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp43: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp81: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp31: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp35: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp14: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp51: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp24: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp90: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp80: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp47: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp92: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp78: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp79: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp48: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp64: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp65: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp19: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp66: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp10: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp89: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp16: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp06: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp26: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp83: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>

  // Same order as in KeyPathReadPerformance, with duplicates removed.
  var kp99: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp27: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp09: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp84: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp22: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp82: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp86: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp49: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp18: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp97: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp69: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp76: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp32: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp52: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp87: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp67: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp12: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp21: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp77: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp40: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp60: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>
  var kp50: WritableKeyPath<FixedSizeArray100<ElementType>, ElementType>

  // Same order as in KeyPathSmallStruct.
  var kp0: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp1: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp2: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp3: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp4: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp5: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp6: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp7: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp8: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>
  var kp9: WritableKeyPath<FixedSizeArray10<ElementType>, ElementType>

  static let shared = FixedSizeArrayHolder()
  init() {
    fixedSizeArray100 = initializeFixedSizeArray100()
    fixedSizeArray10 = initializeFixedSizeArray10()
    mainArrayForNestedStructs = [A]()
    mainArrayForClassStructs = [D1]()
    arrayForMutatingGetset = [MutatingGetsetNested1]()
    arrayForGet = [GetNested1]()
    arrayForOptionals = [Optional1]()
    arrayForNestedClasses = [C1]()
    arrayForNonMutatingGetset = [M]()

    kp46 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 46))
    kp43 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 43))
    kp81 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 81))
    kp31 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 31))
    kp35 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 35))
    kp14 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 14))
    kp51 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 51))
    kp24 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 24))
    kp90 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 90))
    kp80 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 80))
    kp47 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 47))
    kp92 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 92))
    kp78 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 78))
    kp79 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 79))
    kp48 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 48))
    kp64 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 64))
    kp65 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 65))
    kp19 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 19))
    kp66 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 66))
    kp10 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 10))
    kp89 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 89))
    kp16 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 16))
    kp06 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 06))
    kp26 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 26))
    kp83 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 83))

    kp99 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 99))
    kp27 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 27))
    kp09 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 09))
    kp84 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 84))
    kp22 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 22))
    kp82 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 82))
    kp86 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 86))
    kp49 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 49))
    kp18 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 18))
    kp97 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 97))
    kp69 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 69))
    kp76 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 76))
    kp32 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 32))
    kp52 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 52))
    kp24 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 24))
    kp87 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 87))
    kp67 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 67))
    kp65 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 65))
    kp43 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 43))
    kp12 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 12))
    kp21 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 21))
    kp77 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 77))
    kp40 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 40))
    kp60 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 60))
    kp50 = identity(FixedSizeArray100<ElementType>.getKeypathToElement(index: 50))

    kp0 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 0))
    kp1 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 1))
    kp2 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 2))
    kp3 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 3))
    kp4 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 4))
    kp5 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 5))
    kp6 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 6))
    kp7 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 7))
    kp8 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 8))
    kp9 = identity(FixedSizeArray10<ElementType>.getKeypathToElement(index: 9))

    keypathForMutatingGetSet = identity(\MutatingGetsetNested1.nestedItem.nestedItem.nestedItem
      .nestedItem.storage)
    keypathForGet = identity(\GetNested1.nestedItem.nestedItem.nestedItem.nestedItem.storage)
    keypathForOptional = identity(\Optional1._nestedItemStorage?!._nestedItemStorage?!
      ._nestedItemStorage?!._nestedItemStorage?!._storage)
    keypathForNestedClasses = identity(\C1.r.r.r.r.a)
    keypathForNonMutatingGetset = identity(\M.n.o.p.q.q)
    keypathForClassStructs = identity(\D1.b.c.d.e.e)
  }
}

// Setup Functions.
public func setupKeyPathNestedStructs() {
  let holder = FixedSizeArrayHolder.shared
  for _ in 0 ..< numberOfElementsForNestedStructs {
    let instance = A(a: 0, b: B(b: 0, c: C(c: 0, d: D(d: 0, e: E(e: expectedIntForNestedItems)))))
    holder.mainArrayForNestedStructs.append(instance)

    let classStructInstance = D1(b: D2(b: 0, c: D3(c: 0, d: D4(d: 0,
      e: D5(e: expectedIntForNestedItems)))))
    holder.mainArrayForClassStructs.append(classStructInstance)
      
    var mutatingGetsetInstance = MutatingGetsetNested1()
    mutatingGetsetInstance.nestedItem.nestedItem.nestedItem.nestedItem
      .storage = expectedIntForNestedItems
    holder.arrayForMutatingGetset.append(mutatingGetsetInstance)
    holder.arrayForGet.append(GetNested1())
    holder.arrayForOptionals.append(Optional1())
    holder.arrayForNestedClasses.append(C1())
    holder.arrayForNonMutatingGetset
      .append(M(n: N(o: O(p: P(q: Q(q: expectedIntForNestedItems))))))
  }
}

public func setupSingleton() {
  blackHole(FixedSizeArrayHolder.shared)
}

// We would like to compute the sums of the arrays once the iterations finish,
// and doing it this way it likely faster than using keypaths via getKeypathToElement().
func computeSumOfFixedSizeArray100(fixedSizeArray100: inout FixedSizeArray100<ElementType>)
  -> ElementType
{
  var sum = ElementType(0)
  withUnsafeBytes(of: fixedSizeArray100) {
    var pointer = $0.baseAddress.unsafelyUnwrapped
    for _ in 0 ..< fixedSizeArray100.count {
      sum += pointer.assumingMemoryBound(to: ElementType.self).pointee
      pointer = pointer.advanced(by: MemoryLayout<ElementType>.size)
    }
  }
  return sum
}

func computeSumOfFixedSizeArray10(fixedSizeArray10: inout FixedSizeArray10<ElementType>)
  -> ElementType
{
  var sum = ElementType(0)
  withUnsafeBytes(of: fixedSizeArray10) {
    var pointer = $0.baseAddress.unsafelyUnwrapped
    for _ in 0 ..< fixedSizeArray10.count {
      sum += pointer.assumingMemoryBound(to: ElementType.self).pointee
      pointer = pointer.advanced(by: MemoryLayout<ElementType>.size)
    }
  }
  return sum
}

// Used for run_KeyPathNestedStructs().
struct A {
  var a: Int
  var b: B
}

struct B {
  var b: Int
  var c: C
}

struct C {
  var c: Int
  var d: D
}

struct D {
  var d: Int
  var e: E
}

struct E {
  var e: Int
}

// Used for run_KeyPathClassStruct().
class D1 {
  var a: Int
  var b: D2
  init(b: D2)
  {
    a = 0
    self.b = b
  }
}

struct D2 {
  var b: Int
  var c: D3
}

struct D3 {
  var c: Int
  var d: D4
}

struct D4 {
  var d: Int
  var e: D5
}

struct D5 {
  var e: Int
}

// Used for run_KeyPathNestedClasses()
class C1 {
  let a: Int = 0
  let r: C2 = .init()
  class C2 {
    let b: Int = 0
    let r: C3 = .init()
    class C3 {
      let a: Int = 0
      let r: C4 = .init()
      class C4 {
        let a: Int = 0
        let r: C5 = .init()
        class C5 {
          let a: Int = expectedIntForNestedItems
        }
      }
    }
  }
}

// Used for run_KeyPathNonmutatingGetset()
class M {
  var n: N
  init(n: N) { self.n = n }
}

class N {
  var o: O
  init(o: O) { self.o = o }
}

class O {
  var p: P
  init(p: P) { self.p = p }
}

class P {
  var q: Q
  init(q: Q) { self.q = q }
}

class Q {
  var q: Int
  init(q: Int) { self.q = q }
}

// Used for run_KeyPathMutatingGetset()
struct MutatingGetsetNested1 {
  var _storage: Int
  var _nestedItemStorage: MutatingGetsetNested2
  var storage: Int {
    get { return _storage }
    set { _storage = newValue }
  }

  var nestedItem: MutatingGetsetNested2 {
    get { return _nestedItemStorage }
    set { _nestedItemStorage = newValue }
  }

  init() {
    _storage = 0
    _nestedItemStorage = MutatingGetsetNested2()
  }
}

struct MutatingGetsetNested2 {
  var _storage: Int
  var _nestedItemStorage: MutatingGetsetNested3
  var storage: Int {
    get { return _storage }
    set { _storage = newValue }
  }

  var nestedItem: MutatingGetsetNested3 {
    get { return _nestedItemStorage }
    set { _nestedItemStorage = newValue }
  }

  init() {
    _storage = 0
    _nestedItemStorage = MutatingGetsetNested3()
  }
}

struct MutatingGetsetNested3 {
  var _storage: Int
  var _nestedItemStorage: MutatingGetsetNested4
  var storage: Int {
    get { return _storage }
    set { _storage = newValue }
  }

  var nestedItem: MutatingGetsetNested4 {
    get { return _nestedItemStorage }
    set { _nestedItemStorage = newValue }
  }

  init() {
    _storage = 0
    _nestedItemStorage = MutatingGetsetNested4()
  }
}

struct MutatingGetsetNested4 {
  var _storage: Int
  var _nestedItemStorage: MutatingGetsetNested5
  var storage: Int {
    get { return _storage }
    set { _storage = newValue }
  }

  var nestedItem: MutatingGetsetNested5 {
    get { return _nestedItemStorage }
    set { _nestedItemStorage = newValue }
  }

  init() {
    _storage = 0
    _nestedItemStorage = MutatingGetsetNested5()
  }
}

struct MutatingGetsetNested5 {
  var _storage: Int
  var storage: Int {
    get { return _storage }
    set { _storage = newValue }
  }

  init() {
    _storage = 10
  }
}

// Used for run_KeyPathGet().
struct GetNested1 {
  var _storage: Int
  var _nestedItemStorage: GetNested2
  var storage: Int { return _storage }
  var nestedItem: GetNested2 { return _nestedItemStorage }
  init() {
    _storage = 0
    _nestedItemStorage = GetNested2()
  }
}

struct GetNested2 {
  var _storage: Int
  var _nestedItemStorage: GetNested3
  var storage: Int { return _storage }
  var nestedItem: GetNested3 { return _nestedItemStorage }
  init() {
    _storage = 0
    _nestedItemStorage = GetNested3()
  }
}

struct GetNested3 {
  var _storage: Int
  var _nestedItemStorage: GetNested4
  var storage: Int { return _storage }
  var nestedItem: GetNested4 { return _nestedItemStorage }
  init() {
    _storage = 0
    _nestedItemStorage = GetNested4()
  }
}

struct GetNested4 {
  var _storage: Int
  var _nestedItemStorage: GetNested5
  var storage: Int { return _storage }
  var nestedItem: GetNested5 { return _nestedItemStorage }
  init() {
    _storage = 0
    _nestedItemStorage = GetNested5()
  }
}

struct GetNested5 {
  var _storage: Int
  var storage: Int { return _storage }
  init() {
    _storage = expectedIntForNestedItems
  }
}

// Used for run_KeyPathOptionals().
struct Optional1 {
  var _storage: Int
  var _nestedItemStorage: Optional2??
  init() {
    _storage = 0
    _nestedItemStorage = Optional2()
  }
}

struct Optional2 {
  var _storage: Int
  var _nestedItemStorage: Optional3??
  init() {
    _storage = 0
    _nestedItemStorage = Optional3()
  }
}

struct Optional3 {
  var _storage: Int
  var _nestedItemStorage: Optional4??
  init() {
    _storage = 0
    _nestedItemStorage = Optional4()
  }
}

struct Optional4 {
  var _storage: Int
  var _nestedItemStorage: Optional5??
  init() {
    _storage = 0
    _nestedItemStorage = Optional5()
  }
}

struct Optional5 {
  public var _storage: Int
  init() {
    _storage = expectedIntForNestedItems
  }
}

// Used for run_KeyPathFixedSizeArrayAccess() and run_testDirectAccess().
struct FixedSizeArray100<Element>: Sequence, IteratorProtocol {
  let count: Int = 100

  public init(
    element0: Element,
    element1: Element,
    element2: Element,
    element3: Element,
    element4: Element,
    element5: Element,
    element6: Element,
    element7: Element,
    element8: Element,
    element9: Element,
    element10: Element,
    element11: Element,
    element12: Element,
    element13: Element,
    element14: Element,
    element15: Element,
    element16: Element,
    element17: Element,
    element18: Element,
    element19: Element,
    element20: Element,
    element21: Element,
    element22: Element,
    element23: Element,
    element24: Element,
    element25: Element,
    element26: Element,
    element27: Element,
    element28: Element,
    element29: Element,
    element30: Element,
    element31: Element,
    element32: Element,
    element33: Element,
    element34: Element,
    element35: Element,
    element36: Element,
    element37: Element,
    element38: Element,
    element39: Element,
    element40: Element,
    element41: Element,
    element42: Element,
    element43: Element,
    element44: Element,
    element45: Element,
    element46: Element,
    element47: Element,
    element48: Element,
    element49: Element,
    element50: Element,
    element51: Element,
    element52: Element,
    element53: Element,
    element54: Element,
    element55: Element,
    element56: Element,
    element57: Element,
    element58: Element,
    element59: Element,
    element60: Element,
    element61: Element,
    element62: Element,
    element63: Element,
    element64: Element,
    element65: Element,
    element66: Element,
    element67: Element,
    element68: Element,
    element69: Element,
    element70: Element,
    element71: Element,
    element72: Element,
    element73: Element,
    element74: Element,
    element75: Element,
    element76: Element,
    element77: Element,
    element78: Element,
    element79: Element,
    element80: Element,
    element81: Element,
    element82: Element,
    element83: Element,
    element84: Element,
    element85: Element,
    element86: Element,
    element87: Element,
    element88: Element,
    element89: Element,
    element90: Element,
    element91: Element,
    element92: Element,
    element93: Element,
    element94: Element,
    element95: Element,
    element96: Element,
    element97: Element,
    element98: Element,
    element99: Element
  ) {
    self.element0 = element0
    self.element1 = element1
    self.element2 = element2
    self.element3 = element3
    self.element4 = element4
    self.element5 = element5
    self.element6 = element6
    self.element7 = element7
    self.element8 = element8
    self.element9 = element9
    self.element10 = element10
    self.element11 = element11
    self.element12 = element12
    self.element13 = element13
    self.element14 = element14
    self.element15 = element15
    self.element16 = element16
    self.element17 = element17
    self.element18 = element18
    self.element19 = element19
    self.element20 = element20
    self.element21 = element21
    self.element22 = element22
    self.element23 = element23
    self.element24 = element24
    self.element25 = element25
    self.element26 = element26
    self.element27 = element27
    self.element28 = element28
    self.element29 = element29
    self.element30 = element30
    self.element31 = element31
    self.element32 = element32
    self.element33 = element33
    self.element34 = element34
    self.element35 = element35
    self.element36 = element36
    self.element37 = element37
    self.element38 = element38
    self.element39 = element39
    self.element40 = element40
    self.element41 = element41
    self.element42 = element42
    self.element43 = element43
    self.element44 = element44
    self.element45 = element45
    self.element46 = element46
    self.element47 = element47
    self.element48 = element48
    self.element49 = element49
    self.element50 = element50
    self.element51 = element51
    self.element52 = element52
    self.element53 = element53
    self.element54 = element54
    self.element55 = element55
    self.element56 = element56
    self.element57 = element57
    self.element58 = element58
    self.element59 = element59
    self.element60 = element60
    self.element61 = element61
    self.element62 = element62
    self.element63 = element63
    self.element64 = element64
    self.element65 = element65
    self.element66 = element66
    self.element67 = element67
    self.element68 = element68
    self.element69 = element69
    self.element70 = element70
    self.element71 = element71
    self.element72 = element72
    self.element73 = element73
    self.element74 = element74
    self.element75 = element75
    self.element76 = element76
    self.element77 = element77
    self.element78 = element78
    self.element79 = element79
    self.element80 = element80
    self.element81 = element81
    self.element82 = element82
    self.element83 = element83
    self.element84 = element84
    self.element85 = element85
    self.element86 = element86
    self.element87 = element87
    self.element88 = element88
    self.element89 = element89
    self.element90 = element90
    self.element91 = element91
    self.element92 = element92
    self.element93 = element93
    self.element94 = element94
    self.element95 = element95
    self.element96 = element96
    self.element97 = element97
    self.element98 = element98
    self.element99 = element99
  }

  var element0: Element
  var element1: Element
  var element2: Element
  var element3: Element
  var element4: Element
  var element5: Element
  var element6: Element
  var element7: Element
  var element8: Element
  var element9: Element
  var element10: Element
  var element11: Element
  var element12: Element
  var element13: Element
  var element14: Element
  var element15: Element
  var element16: Element
  var element17: Element
  var element18: Element
  var element19: Element
  var element20: Element
  var element21: Element
  var element22: Element
  var element23: Element
  var element24: Element
  var element25: Element
  var element26: Element
  var element27: Element
  var element28: Element
  var element29: Element
  var element30: Element
  var element31: Element
  var element32: Element
  var element33: Element
  var element34: Element
  var element35: Element
  var element36: Element
  var element37: Element
  var element38: Element
  var element39: Element
  var element40: Element
  var element41: Element
  var element42: Element
  var element43: Element
  var element44: Element
  var element45: Element
  var element46: Element
  var element47: Element
  var element48: Element
  var element49: Element
  var element50: Element
  var element51: Element
  var element52: Element
  var element53: Element
  var element54: Element
  var element55: Element
  var element56: Element
  var element57: Element
  var element58: Element
  var element59: Element
  var element60: Element
  var element61: Element
  var element62: Element
  var element63: Element
  var element64: Element
  var element65: Element
  var element66: Element
  var element67: Element
  var element68: Element
  var element69: Element
  var element70: Element
  var element71: Element
  var element72: Element
  var element73: Element
  var element74: Element
  var element75: Element
  var element76: Element
  var element77: Element
  var element78: Element
  var element79: Element
  var element80: Element
  var element81: Element
  var element82: Element
  var element83: Element
  var element84: Element
  var element85: Element
  var element86: Element
  var element87: Element
  var element88: Element
  var element89: Element
  var element90: Element
  var element91: Element
  var element92: Element
  var element93: Element
  var element94: Element
  var element95: Element
  var element96: Element
  var element97: Element
  var element98: Element
  var element99: Element

  @inline(never)
  static func getKeypathToElement(index: Int)
    -> WritableKeyPath<FixedSizeArray100<Element>, Element>
  {
    switch index {
    case 0:
      return \FixedSizeArray100.element0
    case 1:
      return \FixedSizeArray100.element1
    case 2:
      return \FixedSizeArray100.element2
    case 3:
      return \FixedSizeArray100.element3
    case 4:
      return \FixedSizeArray100.element4
    case 5:
      return \FixedSizeArray100.element5
    case 6:
      return \FixedSizeArray100.element6
    case 7:
      return \FixedSizeArray100.element7
    case 8:
      return \FixedSizeArray100.element8
    case 9:
      return \FixedSizeArray100.element9
    case 10:
      return \FixedSizeArray100.element10
    case 11:
      return \FixedSizeArray100.element11
    case 12:
      return \FixedSizeArray100.element12
    case 13:
      return \FixedSizeArray100.element13
    case 14:
      return \FixedSizeArray100.element14
    case 15:
      return \FixedSizeArray100.element15
    case 16:
      return \FixedSizeArray100.element16
    case 17:
      return \FixedSizeArray100.element17
    case 18:
      return \FixedSizeArray100.element18
    case 19:
      return \FixedSizeArray100.element19
    case 20:
      return \FixedSizeArray100.element20
    case 21:
      return \FixedSizeArray100.element21
    case 22:
      return \FixedSizeArray100.element22
    case 23:
      return \FixedSizeArray100.element23
    case 24:
      return \FixedSizeArray100.element24
    case 25:
      return \FixedSizeArray100.element25
    case 26:
      return \FixedSizeArray100.element26
    case 27:
      return \FixedSizeArray100.element27
    case 28:
      return \FixedSizeArray100.element28
    case 29:
      return \FixedSizeArray100.element29
    case 30:
      return \FixedSizeArray100.element30
    case 31:
      return \FixedSizeArray100.element31
    case 32:
      return \FixedSizeArray100.element32
    case 33:
      return \FixedSizeArray100.element33
    case 34:
      return \FixedSizeArray100.element34
    case 35:
      return \FixedSizeArray100.element35
    case 36:
      return \FixedSizeArray100.element36
    case 37:
      return \FixedSizeArray100.element37
    case 38:
      return \FixedSizeArray100.element38
    case 39:
      return \FixedSizeArray100.element39
    case 40:
      return \FixedSizeArray100.element40
    case 41:
      return \FixedSizeArray100.element41
    case 42:
      return \FixedSizeArray100.element42
    case 43:
      return \FixedSizeArray100.element43
    case 44:
      return \FixedSizeArray100.element44
    case 45:
      return \FixedSizeArray100.element45
    case 46:
      return \FixedSizeArray100.element46
    case 47:
      return \FixedSizeArray100.element47
    case 48:
      return \FixedSizeArray100.element48
    case 49:
      return \FixedSizeArray100.element49
    case 50:
      return \FixedSizeArray100.element50
    case 51:
      return \FixedSizeArray100.element51
    case 52:
      return \FixedSizeArray100.element52
    case 53:
      return \FixedSizeArray100.element53
    case 54:
      return \FixedSizeArray100.element54
    case 55:
      return \FixedSizeArray100.element55
    case 56:
      return \FixedSizeArray100.element56
    case 57:
      return \FixedSizeArray100.element57
    case 58:
      return \FixedSizeArray100.element58
    case 59:
      return \FixedSizeArray100.element59
    case 60:
      return \FixedSizeArray100.element60
    case 61:
      return \FixedSizeArray100.element61
    case 62:
      return \FixedSizeArray100.element62
    case 63:
      return \FixedSizeArray100.element63
    case 64:
      return \FixedSizeArray100.element64
    case 65:
      return \FixedSizeArray100.element65
    case 66:
      return \FixedSizeArray100.element66
    case 67:
      return \FixedSizeArray100.element67
    case 68:
      return \FixedSizeArray100.element68
    case 69:
      return \FixedSizeArray100.element69
    case 70:
      return \FixedSizeArray100.element70
    case 71:
      return \FixedSizeArray100.element71
    case 72:
      return \FixedSizeArray100.element72
    case 73:
      return \FixedSizeArray100.element73
    case 74:
      return \FixedSizeArray100.element74
    case 75:
      return \FixedSizeArray100.element75
    case 76:
      return \FixedSizeArray100.element76
    case 77:
      return \FixedSizeArray100.element77
    case 78:
      return \FixedSizeArray100.element78
    case 79:
      return \FixedSizeArray100.element79
    case 80:
      return \FixedSizeArray100.element80
    case 81:
      return \FixedSizeArray100.element81
    case 82:
      return \FixedSizeArray100.element82
    case 83:
      return \FixedSizeArray100.element83
    case 84:
      return \FixedSizeArray100.element84
    case 85:
      return \FixedSizeArray100.element85
    case 86:
      return \FixedSizeArray100.element86
    case 87:
      return \FixedSizeArray100.element87
    case 88:
      return \FixedSizeArray100.element88
    case 89:
      return \FixedSizeArray100.element89
    case 90:
      return \FixedSizeArray100.element90
    case 91:
      return \FixedSizeArray100.element91
    case 92:
      return \FixedSizeArray100.element92
    case 93:
      return \FixedSizeArray100.element93
    case 94:
      return \FixedSizeArray100.element94
    case 95:
      return \FixedSizeArray100.element95
    case 96:
      return \FixedSizeArray100.element96
    case 97:
      return \FixedSizeArray100.element97
    case 98:
      return \FixedSizeArray100.element98
    case 99:
      return \FixedSizeArray100.element99
    default:
      fatalError()
    }
  }

  // Conformance to Iterator.
  mutating func next() -> Element? {
    var iter_count = 0
    if iter_count == count {
      return nil
    } else {
      defer { iter_count += 1 }
      return self[keyPath: FixedSizeArray100.getKeypathToElement(index: iter_count)]
    }
  }
}

// Used for run_testKeyPathSmallStruct().
struct FixedSizeArray10<Element>: Sequence, IteratorProtocol {
  let count: Int = 10

  public init(
    element0: Element,
    element1: Element,
    element2: Element,
    element3: Element,
    element4: Element,
    element5: Element,
    element6: Element,
    element7: Element,
    element8: Element,
    element9: Element

  ) {
    self.element0 = element0
    self.element1 = element1
    self.element2 = element2
    self.element3 = element3
    self.element4 = element4
    self.element5 = element5
    self.element6 = element6
    self.element7 = element7
    self.element8 = element8
    self.element9 = element9
  }

  var element0: Element
  var element1: Element
  var element2: Element
  var element3: Element
  var element4: Element
  var element5: Element
  var element6: Element
  var element7: Element
  var element8: Element
  var element9: Element

  @inline(never)
  static func getKeypathToElement(index: Int)
    -> WritableKeyPath<FixedSizeArray10<Element>, Element>
  {
    switch index {
    case 0:
      return \FixedSizeArray10.element0
    case 1:
      return \FixedSizeArray10.element1
    case 2:
      return \FixedSizeArray10.element2
    case 3:
      return \FixedSizeArray10.element3
    case 4:
      return \FixedSizeArray10.element4
    case 5:
      return \FixedSizeArray10.element5
    case 6:
      return \FixedSizeArray10.element6
    case 7:
      return \FixedSizeArray10.element7
    case 8:
      return \FixedSizeArray10.element8
    case 9:
      return \FixedSizeArray10.element9

    default:
      fatalError()
    }
  }

  // Conformance to Iterator.
  mutating func next() -> Element? {
    var iter_count = 0
    if iter_count == count {
      return nil
    } else {
      defer { iter_count += 1 }
      return self[keyPath: FixedSizeArray10.getKeypathToElement(index: iter_count)]
    }
  }
}

func initializeFixedSizeArray10() -> FixedSizeArray10<ElementType> {
  return FixedSizeArray10<ElementType>(
    element0: 0,
    element1: 1,
    element2: 2,
    element3: 3,
    element4: 4,
    element5: 5,
    element6: 6,
    element7: 7,
    element8: 8,
    element9: 9
  )
}

func initializeFixedSizeArray100() -> FixedSizeArray100<ElementType> {
  return FixedSizeArray100<ElementType>(
    element0: 0,
    element1: 1,
    element2: 2,
    element3: 3,
    element4: 4,
    element5: 5,
    element6: 6,
    element7: 7,
    element8: 8,
    element9: 9,
    element10: 10,
    element11: 11,
    element12: 12,
    element13: 13,
    element14: 14,
    element15: 15,
    element16: 16,
    element17: 17,
    element18: 18,
    element19: 19,
    element20: 20,
    element21: 21,
    element22: 22,
    element23: 23,
    element24: 24,
    element25: 25,
    element26: 26,
    element27: 27,
    element28: 28,
    element29: 29,
    element30: 30,
    element31: 31,
    element32: 32,
    element33: 33,
    element34: 34,
    element35: 35,
    element36: 36,
    element37: 37,
    element38: 38,
    element39: 39,
    element40: 40,
    element41: 41,
    element42: 42,
    element43: 43,
    element44: 44,
    element45: 45,
    element46: 46,
    element47: 47,
    element48: 48,
    element49: 49,
    element50: 50,
    element51: 51,
    element52: 52,
    element53: 53,
    element54: 54,
    element55: 55,
    element56: 56,
    element57: 57,
    element58: 58,
    element59: 59,
    element60: 60,
    element61: 61,
    element62: 62,
    element63: 63,
    element64: 64,
    element65: 65,
    element66: 66,
    element67: 67,
    element68: 68,
    element69: 69,
    element70: 70,
    element71: 71,
    element72: 72,
    element73: 73,
    element74: 74,
    element75: 75,
    element76: 76,
    element77: 77,
    element78: 78,
    element79: 79,
    element80: 80,
    element81: 81,
    element82: 82,
    element83: 83,
    element84: 84,
    element85: 85,
    element86: 86,
    element87: 87,
    element88: 88,
    element89: 89,
    element90: 90,
    element91: 91,
    element92: 92,
    element93: 93,
    element94: 94,
    element95: 95,
    element96: 96,
    element97: 97,
    element98: 98,
    element99: 99
  )
}

// This measures the performance of keypath reads though nested structs.
@inline(never)
public func run_KeyPathNestedStructs(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let singleHopKeyPath0: WritableKeyPath<A, B> = \A.b
  let singleHopKeyPath1: WritableKeyPath<B, C> = \B.c
  let singleHopKeyPath2: WritableKeyPath<C, D> = \C.d
  let singleHopKeyPath3: WritableKeyPath<D, E> = \D.e
  let singleHopKeyPath4: WritableKeyPath<E, Int> = \E.e

  let appendedKeyPath = identity(singleHopKeyPath0.appending(path: singleHopKeyPath1)
    .appending(path: singleHopKeyPath2).appending(path: singleHopKeyPath3)
    .appending(path: singleHopKeyPath4))

  let elementCount = FixedSizeArrayHolder.shared.mainArrayForNestedStructs.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.mainArrayForNestedStructs[index]
    sum += element[keyPath: appendedKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

// This measures the performance of keypath reads where a block of
// trivially-typed memory is preceded by something else (optionals, reference
// types, etc.)
@inline(never)
public func run_KeyPathClassStructs(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 500

  let singleHopKeyPath0: WritableKeyPath<D1, D2> = \D1.b
  let singleHopKeyPath1: WritableKeyPath<D2, D3> = \D2.c
  let singleHopKeyPath2: WritableKeyPath<D3, D4> = \D3.d
  let singleHopKeyPath3: WritableKeyPath<D4, D5> = \D4.e
  let singleHopKeyPath4: WritableKeyPath<D5, Int> = \D5.e

  let appendedKeyPath = identity(singleHopKeyPath0.appending(path: singleHopKeyPath1)
    .appending(path: singleHopKeyPath2).appending(path: singleHopKeyPath3)
    .appending(path: singleHopKeyPath4))

  let elementCount = FixedSizeArrayHolder.shared.mainArrayForClassStructs.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.mainArrayForClassStructs[index]
    sum += element[keyPath: appendedKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}



// This is meant as a baseline, from a timing perspective,
// for run_testKeyPathReadPerformance() and run_testKeyPathWritePerformance().
// It's currently set to ".skip", but is useful for comparing the performance between keypath operations and direct dot-notation.
@inline(never)
public func run_testDirectAccess(n: Int) {
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 125_000
  for t in 1 ... iterationMultipier * n {
    fixedSizeArray100.element50 += fixedSizeArray100.element1 + ElementType(t)
    fixedSizeArray100.element46 += fixedSizeArray100.element63 - fixedSizeArray100.element99
    fixedSizeArray100.element43 += fixedSizeArray100.element39 * fixedSizeArray100.element27
    fixedSizeArray100.element81 += fixedSizeArray100.element49 / fixedSizeArray100.element9
    fixedSizeArray100.element31 += fixedSizeArray100.element90 + fixedSizeArray100.element84
    fixedSizeArray100.element35 += fixedSizeArray100.element6 - fixedSizeArray100.element22
    fixedSizeArray100.element14 += fixedSizeArray100.element67 * fixedSizeArray100.element82
    fixedSizeArray100.element51 += fixedSizeArray100.element40 / fixedSizeArray100.element86
    fixedSizeArray100.element24 += fixedSizeArray100.element23 + fixedSizeArray100.element49
    fixedSizeArray100.element90 += fixedSizeArray100.element95 - fixedSizeArray100.element18
    fixedSizeArray100.element80 += fixedSizeArray100.element45 * fixedSizeArray100.element97
    fixedSizeArray100.element47 += fixedSizeArray100.element65 / fixedSizeArray100.element69
    fixedSizeArray100.element92 += fixedSizeArray100.element80 + fixedSizeArray100.element76
    fixedSizeArray100.element78 += fixedSizeArray100.element32 - fixedSizeArray100.element32
    fixedSizeArray100.element79 += fixedSizeArray100.element59 * fixedSizeArray100.element52
    fixedSizeArray100.element46 += fixedSizeArray100.element60 / fixedSizeArray100.element24
    fixedSizeArray100.element64 += fixedSizeArray100.element41 + fixedSizeArray100.element87
    fixedSizeArray100.element65 += fixedSizeArray100.element72 - fixedSizeArray100.element67
    fixedSizeArray100.element19 += fixedSizeArray100.element81 * fixedSizeArray100.element65
    fixedSizeArray100.element66 += fixedSizeArray100.element55 / fixedSizeArray100.element43
    fixedSizeArray100.element10 += fixedSizeArray100.element52 + fixedSizeArray100.element12
    fixedSizeArray100.element81 += fixedSizeArray100.element50 - fixedSizeArray100.element21
    fixedSizeArray100.element16 += fixedSizeArray100.element80 * fixedSizeArray100.element77
    fixedSizeArray100.element6 += fixedSizeArray100.element62 / fixedSizeArray100.element40
    fixedSizeArray100.element26 += fixedSizeArray100.element65 + fixedSizeArray100.element65
    fixedSizeArray100.element83 += fixedSizeArray100.element78 - fixedSizeArray100.element50
  }
  let sum = computeSumOfFixedSizeArray100(fixedSizeArray100: &fixedSizeArray100)
  check(sum > ElementType(0))
}

// This measures read performance of keypaths.
@inline(never)
public func run_testKeyPathReadPerformance(n: Int) {
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 25

  let kp99 = FixedSizeArrayHolder.shared.kp99
  let kp27 = FixedSizeArrayHolder.shared.kp27
  let kp09 = FixedSizeArrayHolder.shared.kp09
  let kp84 = FixedSizeArrayHolder.shared.kp84
  let kp22 = FixedSizeArrayHolder.shared.kp22
  let kp82 = FixedSizeArrayHolder.shared.kp82
  let kp86 = FixedSizeArrayHolder.shared.kp86
  let kp49 = FixedSizeArrayHolder.shared.kp49
  let kp18 = FixedSizeArrayHolder.shared.kp18
  let kp97 = FixedSizeArrayHolder.shared.kp97
  let kp69 = FixedSizeArrayHolder.shared.kp69
  let kp76 = FixedSizeArrayHolder.shared.kp76
  let kp32 = FixedSizeArrayHolder.shared.kp32
  let kp52 = FixedSizeArrayHolder.shared.kp52
  let kp24 = FixedSizeArrayHolder.shared.kp24
  let kp87 = FixedSizeArrayHolder.shared.kp87
  let kp67 = FixedSizeArrayHolder.shared.kp67
  let kp65 = FixedSizeArrayHolder.shared.kp65
  let kp43 = FixedSizeArrayHolder.shared.kp43
  let kp12 = FixedSizeArrayHolder.shared.kp12
  let kp21 = FixedSizeArrayHolder.shared.kp21
  let kp77 = FixedSizeArrayHolder.shared.kp77
  let kp40 = FixedSizeArrayHolder.shared.kp40
  let kp60 = FixedSizeArrayHolder.shared.kp60
  let kp50 = FixedSizeArrayHolder.shared.kp50

  for t in 1 ... iterationMultipier * n {
    fixedSizeArray100.element50 += fixedSizeArray100.element1 + ElementType(t)
    fixedSizeArray100.element46 += fixedSizeArray100
      .element63 - fixedSizeArray100[keyPath: kp99]
    fixedSizeArray100.element43 += fixedSizeArray100
      .element39 * fixedSizeArray100[keyPath: kp27]
    fixedSizeArray100.element81 += fixedSizeArray100
      .element49 / fixedSizeArray100[keyPath: kp09]
    fixedSizeArray100.element31 += fixedSizeArray100
      .element90 + fixedSizeArray100[keyPath: kp84]
    fixedSizeArray100.element35 += fixedSizeArray100
      .element6 - fixedSizeArray100[keyPath: kp22]
    fixedSizeArray100.element14 += fixedSizeArray100
      .element67 * fixedSizeArray100[keyPath: kp82]
    fixedSizeArray100.element51 += fixedSizeArray100
      .element40 / fixedSizeArray100[keyPath: kp86]
    fixedSizeArray100.element24 += fixedSizeArray100
      .element23 + fixedSizeArray100[keyPath: kp49]
    fixedSizeArray100.element90 += fixedSizeArray100
      .element95 - fixedSizeArray100[keyPath: kp18]
    fixedSizeArray100.element80 += fixedSizeArray100
      .element45 * fixedSizeArray100[keyPath: kp97]
    fixedSizeArray100.element47 += fixedSizeArray100
      .element65 / fixedSizeArray100[keyPath: kp69]
    fixedSizeArray100.element92 += fixedSizeArray100
      .element80 + fixedSizeArray100[keyPath: kp76]
    fixedSizeArray100.element78 += fixedSizeArray100
      .element32 - fixedSizeArray100[keyPath: kp32]
    fixedSizeArray100.element79 += fixedSizeArray100
      .element59 * fixedSizeArray100[keyPath: kp52]
    fixedSizeArray100.element46 += fixedSizeArray100
      .element60 / fixedSizeArray100[keyPath: kp24]
    fixedSizeArray100.element64 += fixedSizeArray100
      .element41 + fixedSizeArray100[keyPath: kp87]
    fixedSizeArray100.element65 += fixedSizeArray100
      .element72 - fixedSizeArray100[keyPath: kp67]
    fixedSizeArray100.element19 += fixedSizeArray100
      .element81 * fixedSizeArray100[keyPath: kp65]
    fixedSizeArray100.element66 += fixedSizeArray100
      .element55 / fixedSizeArray100[keyPath: kp43]
    fixedSizeArray100.element10 += fixedSizeArray100
      .element52 + fixedSizeArray100[keyPath: kp12]
    fixedSizeArray100.element81 += fixedSizeArray100
      .element50 - fixedSizeArray100[keyPath: kp21]
    fixedSizeArray100.element16 += fixedSizeArray100
      .element80 * fixedSizeArray100[keyPath: kp77]
    fixedSizeArray100.element6 += fixedSizeArray100
      .element62 / fixedSizeArray100[keyPath: kp40]
    fixedSizeArray100.element26 += fixedSizeArray100
      .element65 + fixedSizeArray100[keyPath: kp60]
    fixedSizeArray100.element83 += fixedSizeArray100
      .element78 - fixedSizeArray100[keyPath: kp50]
  }
  let sum = computeSumOfFixedSizeArray100(fixedSizeArray100: &fixedSizeArray100)
  check(sum > ElementType(0))
}

// This measures write performance of keypaths.
@inline(never)
public func run_testKeyPathWritePerformance(n: Int) {
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 150

  let kp46 = FixedSizeArrayHolder.shared.kp46
  let kp43 = FixedSizeArrayHolder.shared.kp43
  let kp81 = FixedSizeArrayHolder.shared.kp81
  let kp31 = FixedSizeArrayHolder.shared.kp31
  let kp35 = FixedSizeArrayHolder.shared.kp35
  let kp14 = FixedSizeArrayHolder.shared.kp14
  let kp51 = FixedSizeArrayHolder.shared.kp51
  let kp24 = FixedSizeArrayHolder.shared.kp24
  let kp90 = FixedSizeArrayHolder.shared.kp90
  let kp80 = FixedSizeArrayHolder.shared.kp80
  let kp47 = FixedSizeArrayHolder.shared.kp47
  let kp92 = FixedSizeArrayHolder.shared.kp92
  let kp78 = FixedSizeArrayHolder.shared.kp78
  let kp79 = FixedSizeArrayHolder.shared.kp79
  let kp48 = FixedSizeArrayHolder.shared.kp48
  let kp64 = FixedSizeArrayHolder.shared.kp64
  let kp65 = FixedSizeArrayHolder.shared.kp65
  let kp19 = FixedSizeArrayHolder.shared.kp19
  let kp66 = FixedSizeArrayHolder.shared.kp66
  let kp10 = FixedSizeArrayHolder.shared.kp10
  let kp89 = FixedSizeArrayHolder.shared.kp89
  let kp16 = FixedSizeArrayHolder.shared.kp16
  let kp06 = FixedSizeArrayHolder.shared.kp06
  let kp26 = FixedSizeArrayHolder.shared.kp26
  let kp83 = FixedSizeArrayHolder.shared.kp83

  for t in 1 ... iterationMultipier * n {
    fixedSizeArray100.element50 += fixedSizeArray100.element1 + ElementType(t)
    fixedSizeArray100[keyPath: kp46] += fixedSizeArray100.element63 - fixedSizeArray100
      .element99
    fixedSizeArray100[keyPath: kp43] += fixedSizeArray100.element39 * fixedSizeArray100
      .element27
    fixedSizeArray100[keyPath: kp81] += fixedSizeArray100.element49 / fixedSizeArray100
      .element9
    fixedSizeArray100[keyPath: kp31] += fixedSizeArray100.element90 + fixedSizeArray100
      .element84
    fixedSizeArray100[keyPath: kp35] += fixedSizeArray100.element6 - fixedSizeArray100
      .element22
    fixedSizeArray100[keyPath: kp14] += fixedSizeArray100.element67 * fixedSizeArray100
      .element82
    fixedSizeArray100[keyPath: kp51] += fixedSizeArray100.element40 / fixedSizeArray100
      .element86
    fixedSizeArray100[keyPath: kp24] += fixedSizeArray100.element23 + fixedSizeArray100
      .element49
    fixedSizeArray100[keyPath: kp90] += fixedSizeArray100.element95 - fixedSizeArray100
      .element18
    fixedSizeArray100[keyPath: kp80] += fixedSizeArray100.element45 * fixedSizeArray100
      .element97
    fixedSizeArray100[keyPath: kp47] += fixedSizeArray100.element65 / fixedSizeArray100
      .element69
    fixedSizeArray100[keyPath: kp92] += fixedSizeArray100.element80 + fixedSizeArray100
      .element76
    fixedSizeArray100[keyPath: kp78] += fixedSizeArray100.element32 - fixedSizeArray100
      .element32
    fixedSizeArray100[keyPath: kp79] += fixedSizeArray100.element59 * fixedSizeArray100
      .element52
    fixedSizeArray100[keyPath: kp48] += fixedSizeArray100.element60 / fixedSizeArray100
      .element24
    fixedSizeArray100[keyPath: kp64] += fixedSizeArray100.element41 + fixedSizeArray100
      .element87
    fixedSizeArray100[keyPath: kp65] += fixedSizeArray100.element72 - fixedSizeArray100
      .element67
    fixedSizeArray100[keyPath: kp19] += fixedSizeArray100.element81 * fixedSizeArray100
      .element65
    fixedSizeArray100[keyPath: kp66] += fixedSizeArray100.element55 / fixedSizeArray100
      .element43
    fixedSizeArray100[keyPath: kp10] += fixedSizeArray100.element52 + fixedSizeArray100
      .element12
    fixedSizeArray100[keyPath: kp89] += fixedSizeArray100.element50 - fixedSizeArray100
      .element21
    fixedSizeArray100[keyPath: kp16] += fixedSizeArray100.element80 * fixedSizeArray100
      .element77
    fixedSizeArray100[keyPath: kp06] += fixedSizeArray100.element62 / fixedSizeArray100
      .element40
    fixedSizeArray100[keyPath: kp26] += fixedSizeArray100.element65 + fixedSizeArray100
      .element65
    fixedSizeArray100[keyPath: kp83] += fixedSizeArray100.element78 - fixedSizeArray100
      .element50
  }
  let sum = computeSumOfFixedSizeArray100(fixedSizeArray100: &fixedSizeArray100)
  check(sum > ElementType(0))
}

// Intended to showcase the difference in performance between 10 and 100-element structs.
@inline(never)
public func run_testKeyPathSmallStruct(n: Int) {
  var fixedSizeArray10 = FixedSizeArrayHolder.shared.fixedSizeArray10
  let iterationMultipier = 25

  let kp0 = FixedSizeArrayHolder.shared.kp0
  let kp1 = FixedSizeArrayHolder.shared.kp1
  let kp2 = FixedSizeArrayHolder.shared.kp2
  let kp3 = FixedSizeArrayHolder.shared.kp3
  let kp4 = FixedSizeArrayHolder.shared.kp4
  let kp5 = FixedSizeArrayHolder.shared.kp5
  let kp6 = FixedSizeArrayHolder.shared.kp6
  let kp7 = FixedSizeArrayHolder.shared.kp7
  let kp8 = FixedSizeArrayHolder.shared.kp8
  let kp9 = FixedSizeArrayHolder.shared.kp9

  for t in 1 ... iterationMultipier * n {
    fixedSizeArray10.element0 += fixedSizeArray10.element1 + ElementType(t)
    fixedSizeArray10.element6 += fixedSizeArray10
      .element3 - fixedSizeArray10[keyPath: kp9]
    fixedSizeArray10.element3 += fixedSizeArray10
      .element9 * fixedSizeArray10[keyPath: kp7]
    fixedSizeArray10.element1 += fixedSizeArray10
      .element9 / fixedSizeArray10[keyPath: kp9]
    fixedSizeArray10.element1 += fixedSizeArray10
      .element0 + fixedSizeArray10[keyPath: kp4]
    fixedSizeArray10.element5 += fixedSizeArray10
      .element6 - fixedSizeArray10[keyPath: kp2]
    fixedSizeArray10.element4 += fixedSizeArray10
      .element7 * fixedSizeArray10[keyPath: kp2]
    fixedSizeArray10.element1 += fixedSizeArray10
      .element0 / fixedSizeArray10[keyPath: kp6]
    fixedSizeArray10.element4 += fixedSizeArray10
      .element3 + fixedSizeArray10[keyPath: kp9]
    fixedSizeArray10.element0 += fixedSizeArray10
      .element5 - fixedSizeArray10[keyPath: kp8]
    fixedSizeArray10.element0 += fixedSizeArray10
      .element5 * fixedSizeArray10[keyPath: kp7]
    fixedSizeArray10.element7 += fixedSizeArray10
      .element5 / fixedSizeArray10[keyPath: kp9]
    fixedSizeArray10.element2 += fixedSizeArray10
      .element0 + fixedSizeArray10[keyPath: kp6]
    fixedSizeArray10.element8 += fixedSizeArray10
      .element2 - fixedSizeArray10[keyPath: kp2]
    fixedSizeArray10.element9 += fixedSizeArray10
      .element9 * fixedSizeArray10[keyPath: kp2]
    fixedSizeArray10.element6 += fixedSizeArray10
      .element0 / fixedSizeArray10[keyPath: kp4]
    fixedSizeArray10.element4 += fixedSizeArray10
      .element1 + fixedSizeArray10[keyPath: kp7]
    fixedSizeArray10.element5 += fixedSizeArray10
      .element2 - fixedSizeArray10[keyPath: kp7]
    fixedSizeArray10.element9 += fixedSizeArray10
      .element1 * fixedSizeArray10[keyPath: kp5]
    fixedSizeArray10.element6 += fixedSizeArray10
      .element5 / fixedSizeArray10[keyPath: kp3]
    fixedSizeArray10.element0 += fixedSizeArray10
      .element2 + fixedSizeArray10[keyPath: kp2]
    fixedSizeArray10.element1 += fixedSizeArray10
      .element0 - fixedSizeArray10[keyPath: kp1]
    fixedSizeArray10.element6 += fixedSizeArray10
      .element0 * fixedSizeArray10[keyPath: kp7]
    fixedSizeArray10.element6 += fixedSizeArray10
      .element2 / fixedSizeArray10[keyPath: kp0]
    fixedSizeArray10.element6 += fixedSizeArray10
      .element5 + fixedSizeArray10[keyPath: kp0]
    fixedSizeArray10.element3 += fixedSizeArray10
      .element8 - fixedSizeArray10[keyPath: kp0]
  }
  let sum = computeSumOfFixedSizeArray10(fixedSizeArray10: &fixedSizeArray10)
  blackHole(sum)
}

// Benchmarks traversal through `KeyPathComponents` of type `.mutatingGetset`.
public func run_KeyPathMutatingGetset(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let destinationKeyPath = FixedSizeArrayHolder.shared.keypathForMutatingGetSet
  let elementCount = FixedSizeArrayHolder.shared.arrayForMutatingGetset.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.arrayForMutatingGetset[index]
    sum += element[keyPath: destinationKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

// Benchmarks traversal through `KeyPathComponents` of type `.get`.
public func run_KeyPathGet(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let destinationKeyPath = FixedSizeArrayHolder.shared.keypathForGet
  let elementCount = FixedSizeArrayHolder.shared.arrayForGet.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.arrayForGet[index]
    sum += element[keyPath: destinationKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

// Benchmarks traversal through `KeyPathComponents` of type `.optionalChain,` `.optionalForce,` and `.optionalWrap`.
// Note: The decision that `optionalChains` and `optionalForces` are merged into a single benchmark is arbitrary.
// TODO: Write a benchmark with more than one `.optionalWrap`.
public func run_KeyPathOptionals(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let destinationKeyPath = FixedSizeArrayHolder.shared.keypathForOptional
  let elementCount = FixedSizeArrayHolder.shared.arrayForOptionals.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.arrayForOptionals[index]
    sum += element[keyPath: destinationKeyPath]!
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

// Benchmarks traversal through `KeyPathComponents` of type `.class`.
public func run_KeyPathNestedClasses(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let destinationKeyPath = FixedSizeArrayHolder.shared.keypathForNestedClasses
  let elementCount = FixedSizeArrayHolder.shared.arrayForNestedClasses.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.arrayForNestedClasses[index]
    sum += element[keyPath: destinationKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

// Benchmarks traversal through `KeyPathComponents` of type `.nonmutatingGetset`.
// Note: There may be other, simpler ways of getting consecutive `.nonmutatingGetset`. components.
public func run_KeyPathNonmutatingGetset(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200

  let destinationKeyPath = FixedSizeArrayHolder.shared.keypathForNonMutatingGetset
  let elementCount = FixedSizeArrayHolder.shared.arrayForNonMutatingGetset.count
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.arrayForNonMutatingGetset[index]
    sum += element[keyPath: destinationKeyPath]
    index = (index + 1) % elementCount
  }
  check(sum == iterationMultipier * n * expectedIntForNestedItems)
}

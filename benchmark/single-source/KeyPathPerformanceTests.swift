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
    tags: [.validation, .api],
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
]

// Number of iterations for each benchmark.
let numberOfElementsForNestedStructs = 20000

// Make it easy to switch between primitives of different types (Int, Float, Double, etc.).
typealias ElementType = Double

// Variables used across the benchmarks
let expectedIntForNestedStructs = 3
let arrayHolder = FixedSizeArrayHolder()

let appendedKeyPath = singleHopKeyPath0.appending(path: singleHopKeyPath1)
  .appending(path: singleHopKeyPath2).appending(path: singleHopKeyPath3)
  .appending(path: singleHopKeyPath4)

// The purpose of this class is to hold arrays used during the course of the benchmarks.
// This is to avoid storing them in the global scope, which can slow things down.
// We force instantiation in the setUpFunction so that we exclude the setup time from the benchmark time itself,
// otherwise the instance would be lazily instantiated, and thereby add to the benchmark time.
class FixedSizeArrayHolder {
  var fixedSizeArray100: FixedSizeArray100<ElementType>
  var mainArrayForNestedStructs: [A]
  static let shared = FixedSizeArrayHolder()
  init() {
    fixedSizeArray100 = initializeFixedSizeArray100()
    mainArrayForNestedStructs = [A]()
  }

  func reset() {
    fixedSizeArray100 = initializeFixedSizeArray100()
  }

  func forceInstantiation() {}
}

// Setup Functions.
public func setupKeyPathNestedStructs() {
  FixedSizeArrayHolder.shared.forceInstantiation()
  for _ in 0 ..< numberOfElementsForNestedStructs {
    let instance = A(a: 0, b: B(b: 0, c: C(c: 0, d: D(d: 0, e: E(e: expectedIntForNestedStructs)))))
    FixedSizeArrayHolder.shared.mainArrayForNestedStructs.append(instance)
  }
}

public func setupSingleton() {
  FixedSizeArrayHolder.shared.forceInstantiation()
}

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

let singleHopKeyPath0: WritableKeyPath<A, B> = \A.b
let singleHopKeyPath1: WritableKeyPath<B, C> = \B.c
let singleHopKeyPath2: WritableKeyPath<C, D> = \C.d
let singleHopKeyPath3: WritableKeyPath<D, E> = \D.e
let singleHopKeyPath4: WritableKeyPath<E, Int> = \E.e

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

let kp0 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 0)
let kp1 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 1)
let kp2 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 2)
let kp3 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 3)
let kp4 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 4)
let kp5 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 5)
let kp6 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 6)
let kp7 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 7)
let kp8 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 8)
let kp9 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 9)
let kp10 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 10)
let kp11 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 11)
let kp12 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 12)
let kp13 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 13)
let kp14 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 14)
let kp15 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 15)
let kp16 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 16)
let kp17 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 17)
let kp18 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 18)
let kp19 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 19)
let kp20 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 20)
let kp21 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 21)
let kp22 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 22)
let kp23 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 23)
let kp24 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 24)
let kp25 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 25)
let kp26 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 26)
let kp27 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 27)
let kp28 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 28)
let kp29 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 29)
let kp30 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 30)
let kp31 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 31)
let kp32 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 32)
let kp33 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 33)
let kp34 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 34)
let kp35 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 35)
let kp36 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 36)
let kp37 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 37)
let kp38 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 38)
let kp39 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 39)
let kp40 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 40)
let kp41 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 41)
let kp42 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 42)
let kp43 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 43)
let kp44 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 44)
let kp45 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 45)
let kp46 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 46)
let kp47 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 47)
let kp48 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 48)
let kp49 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 49)
let kp50 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 50)
let kp51 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 51)
let kp52 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 52)
let kp53 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 53)
let kp54 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 54)
let kp55 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 55)
let kp56 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 56)
let kp57 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 57)
let kp58 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 58)
let kp59 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 59)
let kp60 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 60)
let kp61 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 61)
let kp62 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 62)
let kp63 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 63)
let kp64 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 64)
let kp65 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 65)
let kp66 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 66)
let kp67 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 67)
let kp68 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 68)
let kp69 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 69)
let kp70 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 70)
let kp71 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 71)
let kp72 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 72)
let kp73 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 73)
let kp74 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 74)
let kp75 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 75)
let kp76 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 76)
let kp77 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 77)
let kp78 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 78)
let kp79 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 79)
let kp80 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 80)
let kp81 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 81)
let kp82 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 82)
let kp83 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 83)
let kp84 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 84)
let kp85 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 85)
let kp86 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 86)
let kp87 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 87)
let kp88 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 88)
let kp89 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 89)
let kp90 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 90)
let kp91 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 91)
let kp92 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 92)
let kp93 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 93)
let kp94 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 94)
let kp95 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 95)
let kp96 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 96)
let kp97 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 97)
let kp98 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 98)
let kp99 = FixedSizeArray100<ElementType>.getKeypathToElement(index: 99)

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

func returnKeyPathArray() -> [AnyKeyPath] {
  var arrayOfKeyPaths = [kp0, kp1, kp2, kp3, kp4, kp5, kp6, kp7, kp8, kp9]
  arrayOfKeyPaths.append(contentsOf: [kp10, kp11, kp12, kp13, kp14, kp15, kp16, kp17, kp18, kp19])
  arrayOfKeyPaths.append(contentsOf: [kp20, kp21, kp22, kp23, kp24, kp25, kp26, kp27, kp28, kp29])
  arrayOfKeyPaths.append(contentsOf: [kp30, kp31, kp32, kp33, kp34, kp35, kp36, kp37, kp38, kp39])
  arrayOfKeyPaths.append(contentsOf: [kp40, kp41, kp42, kp43, kp44, kp45, kp46, kp47, kp48, kp49])
  arrayOfKeyPaths.append(contentsOf: [kp50, kp51, kp52, kp53, kp54, kp55, kp56, kp57, kp58, kp59])
  arrayOfKeyPaths.append(contentsOf: [kp60, kp61, kp62, kp63, kp64, kp65, kp66, kp67, kp68, kp69])
  arrayOfKeyPaths.append(contentsOf: [kp70, kp71, kp72, kp73, kp74, kp75, kp76, kp77, kp78, kp79])
  arrayOfKeyPaths.append(contentsOf: [kp80, kp81, kp82, kp83, kp84, kp85, kp86, kp87, kp88, kp89])
  arrayOfKeyPaths.append(contentsOf: [kp90, kp91, kp92, kp93, kp94, kp95, kp96, kp97, kp98, kp99])
  return arrayOfKeyPaths
}

@inline(never)
public func run_KeyPathNestedStructs(n: Int) {
  var sum = 0
  var index = 0
  let iterationMultipier = 200
  for _ in 1 ... iterationMultipier * n {
    let element = FixedSizeArrayHolder.shared.mainArrayForNestedStructs[index]
    sum += element[keyPath: appendedKeyPath]
    index = (index + 1) % FixedSizeArrayHolder.shared.mainArrayForNestedStructs.count
  }
  assert(sum == iterationMultipier * n * expectedIntForNestedStructs)
}

// This is meant as a baseline, from a timing perspective,
// for run_testKeyPathReadPerformance() and run_testKeyPathWritePerformance().
@inline(never)
public func run_testDirectAccess(n: Int) {
  arrayHolder.reset()
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 75000
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
  assert(sum > ElementType(0))
}

// This measures read performance of keypaths.
@inline(never)
public func run_testKeyPathReadPerformance(n: Int) {
  arrayHolder.reset()
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 25
  for t in 1 ... iterationMultipier * n {
    fixedSizeArray100.element50 += fixedSizeArray100.element1 + ElementType(t)
    fixedSizeArray100.element46 += fixedSizeArray100.element63 - fixedSizeArray100[keyPath: kp99]
    fixedSizeArray100.element43 += fixedSizeArray100.element39 * fixedSizeArray100[keyPath: kp27]
    fixedSizeArray100.element81 += fixedSizeArray100.element49 / fixedSizeArray100[keyPath: kp9]
    fixedSizeArray100.element31 += fixedSizeArray100.element90 + fixedSizeArray100[keyPath: kp84]
    fixedSizeArray100.element35 += fixedSizeArray100.element6 - fixedSizeArray100[keyPath: kp22]
    fixedSizeArray100.element14 += fixedSizeArray100.element67 * fixedSizeArray100[keyPath: kp82]
    fixedSizeArray100.element51 += fixedSizeArray100.element40 / fixedSizeArray100[keyPath: kp86]
    fixedSizeArray100.element24 += fixedSizeArray100.element23 + fixedSizeArray100[keyPath: kp49]
    fixedSizeArray100.element90 += fixedSizeArray100.element95 - fixedSizeArray100[keyPath: kp18]
    fixedSizeArray100.element80 += fixedSizeArray100.element45 * fixedSizeArray100[keyPath: kp97]
    fixedSizeArray100.element47 += fixedSizeArray100.element65 / fixedSizeArray100[keyPath: kp69]
    fixedSizeArray100.element92 += fixedSizeArray100.element80 + fixedSizeArray100[keyPath: kp76]
    fixedSizeArray100.element78 += fixedSizeArray100.element32 - fixedSizeArray100[keyPath: kp32]
    fixedSizeArray100.element79 += fixedSizeArray100.element59 * fixedSizeArray100[keyPath: kp52]
    fixedSizeArray100.element46 += fixedSizeArray100.element60 / fixedSizeArray100[keyPath: kp24]
    fixedSizeArray100.element64 += fixedSizeArray100.element41 + fixedSizeArray100[keyPath: kp87]
    fixedSizeArray100.element65 += fixedSizeArray100.element72 - fixedSizeArray100[keyPath: kp67]
    fixedSizeArray100.element19 += fixedSizeArray100.element81 * fixedSizeArray100[keyPath: kp65]
    fixedSizeArray100.element66 += fixedSizeArray100.element55 / fixedSizeArray100[keyPath: kp43]
    fixedSizeArray100.element10 += fixedSizeArray100.element52 + fixedSizeArray100[keyPath: kp12]
    fixedSizeArray100.element81 += fixedSizeArray100.element50 - fixedSizeArray100[keyPath: kp21]
    fixedSizeArray100.element16 += fixedSizeArray100.element80 * fixedSizeArray100[keyPath: kp77]
    fixedSizeArray100.element6 += fixedSizeArray100.element62 / fixedSizeArray100[keyPath: kp40]
    fixedSizeArray100.element26 += fixedSizeArray100.element65 + fixedSizeArray100[keyPath: kp65]
    fixedSizeArray100.element83 += fixedSizeArray100.element78 - fixedSizeArray100[keyPath: kp50]
  }
  let sum = computeSumOfFixedSizeArray100(fixedSizeArray100: &fixedSizeArray100)
  assert(sum > ElementType(0))
}

// This measures write performance of keypaths.
@inline(never)
public func run_testKeyPathWritePerformance(n: Int) {
  arrayHolder.reset()
  var fixedSizeArray100 = FixedSizeArrayHolder.shared.fixedSizeArray100
  let iterationMultipier = 150
  for t in 1 ... iterationMultipier * n {
    fixedSizeArray100.element50 += fixedSizeArray100.element1 + ElementType(t)
    fixedSizeArray100[keyPath: kp46] += fixedSizeArray100.element63 - fixedSizeArray100.element99
    fixedSizeArray100[keyPath: kp43] += fixedSizeArray100.element39 * fixedSizeArray100.element27
    fixedSizeArray100[keyPath: kp81] += fixedSizeArray100.element49 / fixedSizeArray100.element9
    fixedSizeArray100[keyPath: kp31] += fixedSizeArray100.element90 + fixedSizeArray100.element84
    fixedSizeArray100[keyPath: kp35] += fixedSizeArray100.element6 - fixedSizeArray100.element22
    fixedSizeArray100[keyPath: kp14] += fixedSizeArray100.element67 * fixedSizeArray100.element82
    fixedSizeArray100[keyPath: kp51] += fixedSizeArray100.element40 / fixedSizeArray100.element86
    fixedSizeArray100[keyPath: kp24] += fixedSizeArray100.element23 + fixedSizeArray100.element49
    fixedSizeArray100[keyPath: kp90] += fixedSizeArray100.element95 - fixedSizeArray100.element18
    fixedSizeArray100[keyPath: kp80] += fixedSizeArray100.element45 * fixedSizeArray100.element97
    fixedSizeArray100[keyPath: kp47] += fixedSizeArray100.element65 / fixedSizeArray100.element69
    fixedSizeArray100[keyPath: kp92] += fixedSizeArray100.element80 + fixedSizeArray100.element76
    fixedSizeArray100[keyPath: kp78] += fixedSizeArray100.element32 - fixedSizeArray100.element32
    fixedSizeArray100[keyPath: kp79] += fixedSizeArray100.element59 * fixedSizeArray100.element52
    fixedSizeArray100[keyPath: kp46] += fixedSizeArray100.element60 / fixedSizeArray100.element24
    fixedSizeArray100[keyPath: kp64] += fixedSizeArray100.element41 + fixedSizeArray100.element87
    fixedSizeArray100[keyPath: kp65] += fixedSizeArray100.element72 - fixedSizeArray100.element67
    fixedSizeArray100[keyPath: kp19] += fixedSizeArray100.element81 * fixedSizeArray100.element65
    fixedSizeArray100[keyPath: kp66] += fixedSizeArray100.element55 / fixedSizeArray100.element43
    fixedSizeArray100[keyPath: kp10] += fixedSizeArray100.element52 + fixedSizeArray100.element12
    fixedSizeArray100[keyPath: kp81] += fixedSizeArray100.element50 - fixedSizeArray100.element21
    fixedSizeArray100[keyPath: kp16] += fixedSizeArray100.element80 * fixedSizeArray100.element77
    fixedSizeArray100[keyPath: kp6] += fixedSizeArray100.element62 / fixedSizeArray100.element40
    fixedSizeArray100[keyPath: kp26] += fixedSizeArray100.element65 + fixedSizeArray100.element65
    fixedSizeArray100[keyPath: kp83] += fixedSizeArray100.element78 - fixedSizeArray100.element50
  }
  let sum = computeSumOfFixedSizeArray100(fixedSizeArray100: &fixedSizeArray100)
  assert(sum > ElementType(0))
}

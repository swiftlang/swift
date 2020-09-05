//===--- FloatingPointConversion.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let FloatingPointConversion = [
  BenchmarkInfo(
    name: "ConvertFloatingPoint.ConcreteDoubleToDouble",
    runFunction: run_ConvertFloatingPoint_ConcreteDoubleToDouble,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "ConvertFloatingPoint.GenericDoubleToDouble",
    runFunction: run_ConvertFloatingPoint_GenericDoubleToDouble,
    tags: [.validation, .api]),
]

let doubles = [
   1.8547832857295,  26.321549267719135, 98.9544480962058,    73.70286973782363,
  82.04918555938816, 76.38902969312758,  46.35647857011161,   64.0821426030317,
  97.82373347320156, 55.742361037720634, 23.677941665488856,  93.7347588108058,
  80.72657040828412, 32.137580733275826, 64.78192587530002,   21.459686568896863,
  24.88407660280718, 85.25905561999171,  12.858847331083556,  29.418845887252864,
  67.64627066438761, 68.09883494078815,  57.781587230862094,  63.38335631088038,
  83.31376661495327, 87.45936846358906,   0.6757674136841918, 86.45465036820696,
  84.72715137492781, 82.67894289189142,  26.1667640621554,    21.24895661442493,
  65.06399183516027, 90.06549073883058,  59.2736650501005,    94.5800380563246,
  84.22617424003917, 26.93158630395639,   9.069952095976841,  96.10067836567679,
  62.60505762081415, 29.57878462599286,  66.06040114311294,   51.709999429326636,
  64.79777579583545, 45.25948795832151,  94.31492354198335,   52.31096166433902,
]

@inline(__always)
func convert<
  T: BinaryFloatingPoint, U: BinaryFloatingPoint
>(_ value: T, to: U.Type) -> U {
  U(value)
}

@inline(never)
public func run_ConvertFloatingPoint_ConcreteDoubleToDouble(_ N: Int) {
  for _ in 0..<(N * 100) {
    for element in doubles {
      let f = Double(identity(element))
      blackHole(f)
    }
  }
}

@inline(never)
public func run_ConvertFloatingPoint_GenericDoubleToDouble(_ N: Int) {
  for _ in 0..<(N * 100) {
    for element in doubles {
      let f = convert(identity(element), to: Double.self)
      blackHole(f)
    }
  }
}

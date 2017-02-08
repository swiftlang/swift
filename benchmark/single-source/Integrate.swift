//===--- Integrate.swift --------------------------------------------------===//
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

// A micro-benchmark for recursive divide and conquer problems.
// The program performs integration via Gaussian Quadrature

class Integrate {
  static let epsilon = 1.0e-9

  let fun: (Double) -> Double

  init (f: @escaping (Double) -> Double) {
    fun = f
  }
    
  private func recEval(_ l: Double, fl: Double, r: Double, fr: Double, a: Double) -> Double {
    let h = (r - l) / 2
    let hh = h / 2
    let c = l + h
    let fc = fun(c)
    let al = (fl + fc) * hh
    let ar = (fr + fc) * hh
    let alr = al + ar
    let error = abs(alr-a)
    if (error < Integrate.epsilon) {
      return alr
    } else {
      let a1 = recEval(c, fl:fc, r:r, fr:fr, a:ar)
      let a2 = recEval(l, fl:fl, r:c, fr:fc, a:al)
      return a1 + a2
    }
  }

  @inline(never)
  func computeArea(_ left: Double, right: Double) -> Double {
    return recEval(left, fl:fun(left), r:right, fr:fun(right), a:0)
  }
}

@inline(never)
public func run_Integrate(_ N: Int) {
  let obj = Integrate(f: { x in (x*x + 1.0) * x})
  let left = 0.0
  let right = 10.0
  let ref_result = 2550.0
  let bound = 0.0001
  var result = 0.0
  for _ in 1...N {
    result = obj.computeArea(left, right:right)
    if abs(result - ref_result) > bound {
      break
    }
  }

  CheckResults(abs(result - ref_result) < bound,
               "Incorrect results in Integrate: abs(\(result) - \(ref_result)) > \(bound)")
}

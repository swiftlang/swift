//===--- StrToInt.swift ---------------------------------------------------===//
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

// This test checks performance of String to Int conversion.
// It is reported to be very slow: <rdar://problem/17255477>
import TestsUtils

@inline(never)
public func run_StrToInt(_ N: Int) {
  // 64 numbers from -500_000 to 500_000 generated randomly
  let input = ["-237392", "293715", "126809", "333779", "-362824", "144198",
               "-394973", "-163669", "-7236", "376965", "-400783", "-118670",
               "454728", "-38915", "136285", "-448481", "-499684", "68298",
               "382671", "105432", "-38385", "39422", "-267849", "-439886",
               "292690", "87017", "404692", "27692", "486408", "336482",
               "-67850", "56414", "-340902", "-391782", "414778", "-494338",
               "-413017", "-377452", "-300681", "170194", "428941", "-291665",
               "89331", "329496", "-364449", "272843", "-10688", "142542",
               "-417439", "167337", "96598", "-264104", "-186029", "98480",
               "-316727", "483808", "300149", "-405877", "-98938", "283685",
               "-247856", "-46975", "346060", "160085",]
  let ref_result = 517492
  func DoOneIter(_ arr: [String]) -> Int {
    var r = 0
    for n in arr {
      r += Int(n)!
    }
    if r < 0 {
      r = -r
    }
    return r
  }
  var res = Int.max
  for _ in 1...1000*N {
    res = res & DoOneIter(input)
  }
  CheckResults(res == ref_result, "IncorrectResults in StrToInt: \(res) != \(ref_result)")
}

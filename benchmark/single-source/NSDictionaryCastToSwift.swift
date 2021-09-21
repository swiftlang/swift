//===--- NSDictionaryCastToSwift.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Performance benchmark for casting NSDictionary to Swift Dictionary
// rdar://problem/18539730
//
// Description:
//     Create an NSDictionary instance and cast it to [String: NSObject].
import Foundation
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "NSDictionaryCastToSwift",
    runFunction: run_NSDictionaryCastToSwift,
    tags: [.validation, .api, .Dictionary, .bridging],
    legacyFactor: 10)

@inline(never)
public func run_NSDictionaryCastToSwift(_ n: Int) {
#if _runtime(_ObjC)
    let nsdict = NSDictionary()
    var swiftDict = [String: NSObject]()
    for _ in 1...1_000*n {
        swiftDict = nsdict as! [String: NSObject]
        if !swiftDict.isEmpty {
            break
        }
    }
    check(swiftDict.isEmpty)
#endif
}

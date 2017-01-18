//===--- NSDictionaryCastToSwift.swift ------------------------------------===//
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

// Performance benchmark for casting NSDictionary to Swift Dictionary
// rdar://problem/18539730
//
// Description:
//     Create an NSDictionary instance and cast it to [String: NSObject].
import Foundation
import TestsUtils

@inline(never)
public func run_NSDictionaryCastToSwift(_ N: Int) {
    let NSDict = NSDictionary()
    var swiftDict = [String: NSObject]()
    for _ in 1...10000*N {
        swiftDict = NSDict as! [String: NSObject]
        if !swiftDict.isEmpty {
            break
        }
    }
    CheckResults(swiftDict.isEmpty,
            "Incorrect result in swiftDict.isEmpty: " +
            "\(swiftDict.isEmpty) != true\n")
}

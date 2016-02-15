//===--- DictionaryRemove.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Dictionary element removal benchmark
// rdar://problem/19804127
import TestsUtils

@inline(never)
public func run_DictionaryRemove(N: Int) {
    let size = 100
    var dict = [Int: Int](minimumCapacity: size)

    // Fill dictionary
    for i in 1...size {
        dict[i] = i
    }
    CheckResults(dict.count == size,
                 "Incorrect dict count: \(dict.count) != \(size).")

    var tmpDict = dict
    for _ in 1...1000*N {
        tmpDict = dict
        // Empty dictionary
        for i in 1...size {
            tmpDict.removeValueForKey(i)
        }
        if !tmpDict.isEmpty {
            break
        }
    }

    CheckResults(tmpDict.isEmpty,
                 "tmpDict should be empty: \(tmpDict.count) != 0.")
}

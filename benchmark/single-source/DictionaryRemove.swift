//===--- DictionaryRemove.swift -------------------------------------------===//
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

// Dictionary element removal benchmark
// rdar://problem/19804127
import TestsUtils

@inline(never)
public func run_DictionaryRemove(_ N: Int) {
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
            tmpDict.removeValue(forKey: i)
        }
        if !tmpDict.isEmpty {
            break
        }
    }

    CheckResults(tmpDict.isEmpty,
                 "tmpDict should be empty: \(tmpDict.count) != 0.")
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  var hashValue: Int {
    return value.hashValue
  }

  static func ==<T: Equatable>(lhs: Box<T>, rhs: Box<T>) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_DictionaryRemoveOfObjects(_ N: Int) {
    let size = 100
    var dict = Dictionary<Box<Int>, Box<Int>>(minimumCapacity: size)

    // Fill dictionary
    for i in 1...size {
        dict[Box(i)] = Box(i)
    }
    CheckResults(dict.count == size,
                 "Incorrect dict count: \(dict.count) != \(size).")

    var tmpDict = dict
    for _ in 1...1000*N {
        tmpDict = dict
        // Empty dictionary
        for i in 1...size {
            tmpDict.removeValue(forKey: Box(i))
        }
        if !tmpDict.isEmpty {
            break
        }
    }

    CheckResults(tmpDict.isEmpty,
                 "tmpDict should be empty: \(tmpDict.count) != 0.")
}

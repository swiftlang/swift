//===--- ArrayAppend.swift ------------------------------------------------===//
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

// This test checks the performance of appending to an array.

import TestsUtils

public let ArrayAppend = [
  BenchmarkInfo(name: "ArrayAppend", runFunction: run_ArrayAppend, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendArrayOfInt", runFunction: run_ArrayAppendArrayOfInt, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendAscii", runFunction: run_ArrayAppendAscii, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendAsciiSubstring", runFunction: run_ArrayAppendAsciiSubstring, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendFromGeneric", runFunction: run_ArrayAppendFromGeneric, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendGenericStructs", runFunction: run_ArrayAppendGenericStructs, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendLatin1", runFunction: run_ArrayAppendLatin1, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendLatin1Substring", runFunction: run_ArrayAppendLatin1Substring, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendLazyMap", runFunction: run_ArrayAppendLazyMap, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendOptionals", runFunction: run_ArrayAppendOptionals, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendRepeatCol", runFunction: run_ArrayAppendRepeatCol, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendReserved", runFunction: run_ArrayAppendReserved, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendSequence", runFunction: run_ArrayAppendSequence, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendStrings", runFunction: run_ArrayAppendStrings, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendToFromGeneric", runFunction: run_ArrayAppendToFromGeneric, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendToGeneric", runFunction: run_ArrayAppendToGeneric, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendUTF16", runFunction: run_ArrayAppendUTF16, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayAppendUTF16Substring", runFunction: run_ArrayAppendUTF16Substring, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayPlusEqualArrayOfInt", runFunction: run_ArrayPlusEqualArrayOfInt, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayPlusEqualFiveElementCollection", runFunction: run_ArrayPlusEqualFiveElementCollection, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayPlusEqualSingleElementCollection", runFunction: run_ArrayPlusEqualSingleElementCollection, tags: [.validation, .api, .Array]),
  BenchmarkInfo(name: "ArrayPlusEqualThreeElements", runFunction: run_ArrayPlusEqualThreeElements, tags: [.validation, .api, .Array]),
]

// Append single element
@inline(never)
public func run_ArrayAppend(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}

// Append single element with reserve
@inline(never)
public func run_ArrayAppendReserved(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       nums.reserveCapacity(40000)
       for _ in 0..<40000 {
         nums.append(1)
       }
    }
  }
}

// Append a sequence. Length of sequence unknown so
// can't pre-reserve capacity.
@inline(never)
public func run_ArrayAppendSequence(_ N: Int) {
  let seq = stride(from: 0, to: 10_000, by: 1)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums.append(contentsOf: seq)
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendArrayOfInt(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums.append(contentsOf: other)
      }
    }
  }
}

// Identical to run_ArrayAppendArrayOfInt
// except +=, to check equally performant.
@inline(never)
public func run_ArrayPlusEqualArrayOfInt(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendStrings(_ N: Int) {
  let other = stride(from: 0, to: 10_000, by: 1).map { "\($0)" }
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [String]()
      // lower inner count due to string slowness
      for _ in 0..<4 {
        nums += other
      }
    }
  }
}

struct S<T,U> {
  var x: T
  var y: U
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendGenericStructs(_ N: Int) {
  let other = Array(repeating: S(x: 3, y: 4.2), count: 10_000)
  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [S<Int, Double>]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}

// Append another array. Length of sequence known so
// can pre-reserve capacity.
@inline(never)
public func run_ArrayAppendOptionals(_ N: Int) {
  let other: [Int?] = Array(repeating: 1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int?]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


// Append a lazily-mapped array. Length of sequence known so
// can pre-reserve capacity, but no optimization points used.
@inline(never)
public func run_ArrayAppendLazyMap(_ N: Int) {
  let other = Array(0..<10_000).lazy.map { $0 * 2 }

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


// Append a Repeat collection. Length of sequence known so
// can pre-reserve capacity, but no optimization points used.
@inline(never)
public func run_ArrayAppendRepeatCol(_ N: Int) {
  let other = repeatElement(1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        nums += other
      }
    }
  }
}


// Append an array as a generic sequence to another array
@inline(never)
public func appendFromGeneric<
  S: Sequence
>(array: inout [S.Element], sequence: S) {
  array.append(contentsOf: sequence)
}

@inline(never)
public func run_ArrayAppendFromGeneric(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        appendFromGeneric(array: &nums, sequence: other)
      }
    }
  }
}

// Append an array to an array as a generic range replaceable collection.
@inline(never)
public func appendToGeneric<
  R: RangeReplaceableCollection
>(collection: inout R, array: [R.Element]) {
  collection.append(contentsOf: array)
}

@inline(never)
public func run_ArrayAppendToGeneric(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        appendToGeneric(collection: &nums, array: other)
      }
    }
  }
}

// Append an array as a generic sequence to an array as a generic range
// replaceable collection.
@inline(never)
public func appendToFromGeneric<
  R: RangeReplaceableCollection, S: Sequence
>(collection: inout R, sequence: S)
where R.Element == S.Element {
  collection.append(contentsOf: sequence)
}

@inline(never)
public func run_ArrayAppendToFromGeneric(_ N: Int) {
  let other = Array(repeating: 1, count: 10_000)

  for _ in 0..<N {
    for _ in 0..<10 {
      var nums = [Int]()
      for _ in 0..<8 {
        appendToFromGeneric(collection: &nums, sequence: other)
      }
    }
  }
}

// Append a single element array with the += operator
@inline(never)
public func run_ArrayPlusEqualSingleElementCollection(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40_000 {
         nums += [1]
       }
    }
  }
}

// Append a five element array with the += operator
@inline(never)
public func run_ArrayPlusEqualFiveElementCollection(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40_000 {
         nums += [1, 2, 3, 4, 5]
       }
    }
  }
}

@inline(never)
public func appendThreeElements(_ a: inout [Int]) {
  a += [1, 2, 3]
}

@inline(never)
public func run_ArrayPlusEqualThreeElements(_ N: Int) {
  for _ in 0..<(10_000 * N) {
    var a: [Int] = []
    appendThreeElements(&a)
  }
}

// Append the utf8 elements of an ascii string to a [UInt8]
@inline(never)
public func run_ArrayAppendAscii(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog!"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getString(s).utf8
       }
    }
  }
}

// Append the utf8 elements of a latin1 string to a [UInt8]
@inline(never)
public func run_ArrayAppendLatin1(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog\u{00A1}"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getString(s).utf8
       }
    }
  }
}

// Append the utf8 elements of an utf16 string to a [UInt8]
@inline(never)
public func run_ArrayAppendUTF16(_ N: Int) {
  let s = "the quick brown 🦊 jumps over the lazy dog"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getString(s).utf8
       }
    }
  }
}

// Append the utf8 elements of an ascii substring to a [UInt8]
@inline(never)
public func run_ArrayAppendAsciiSubstring(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog!"[...]
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getSubstring(s).utf8
       }
    }
  }
}

// Append the utf8 elements of a latin1 substring to a [UInt8]
@inline(never)
public func run_ArrayAppendLatin1Substring(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog\u{00A1}"[...]
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getSubstring(s).utf8
       }
    }
  }
}

// Append the utf8 elements of an utf16 substring to a [UInt8]
@inline(never)
public func run_ArrayAppendUTF16Substring(_ N: Int) {
  let s = "the quick brown 🦊 jumps over the lazy dog"[...]
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += getSubstring(s).utf8
       }
    }
  }
}

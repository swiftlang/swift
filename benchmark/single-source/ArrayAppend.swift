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
      var nums = [S<Int,Double>]()
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
>(array: inout [S.Iterator.Element], sequence: S) {
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
>(collection: inout R, array: [R.Iterator.Element]) {
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
where R.Iterator.Element == S.Iterator.Element {
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

// Append the utf8 elements of an ascii string to a [UInt8]
@inline(never)
public func run_ArrayAppendAscii(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog!"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += s.utf8
       }
    }
  }
}

// Append the utf8 elements of an ascii string to a [UInt8]
@inline(never)
public func run_ArrayAppendLatin1(_ N: Int) {
  let s = "the quick brown fox jumps over the lazy dog\u{00A1}"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += s.utf8
       }
    }
  }
}

// Append the utf8 elements of an ascii string to a [UInt8]
@inline(never)
public func run_ArrayAppendUTF16(_ N: Int) {
  let s = "the quick brown 🦊 jumps over the lazy dog"
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [UInt8]()
       for _ in 0..<10_000 {
         nums += s.utf8
       }
    }
  }
}


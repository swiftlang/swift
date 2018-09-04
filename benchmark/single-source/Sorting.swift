//===--- Sorting.swift ----------------------------------------------------===//
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

// This test exercises the standard library sort on different shapes of data,
// running the sort on three different kinds of types:
//
// - a small struct: `Int`
// - a large struct: `LargeComparable`
// - a reference type: `IntBox`

import TestsUtils

/// Integers in order, with no collisions
func inOrder(_ size: Int) -> [Int] {
    return Array(0..<size)
}

/// Integers in order, plus a handful of random new additions at the end
func inOrderWithAppends(_ size: Int) -> [Int] {
    let appendSize = max(11, size / 100)
    return Array(0..<(size - appendSize))
        + Array(0..<appendSize).map({ _ in Int.random(in: 0..<size - appendSize)})
}

/// Integers in order, plus a handful of random new additions at the beginning
func inOrderWithPrepends(_ size: Int) -> [Int] {
    let prependSize = max(11, size / 100)
    return Array(0..<prependSize).map({ _ in Int.random(in: 0..<size - prependSize)})
        + Array(0..<(size - prependSize))
}

/// Integers in order, plus a handful of random new additions inserted at
/// random positions
func inOrderWithSprinkles(_ size: Int) -> [Int] {
    let sprinkleSize = max(11, size / 100)
    var result = Array(0..<size)
    for _ in 0..<sprinkleSize {
        result[Int.random(in: 0..<size)] = Int.random(in: 0..<size)
    }
    return result
}

/// Returns a sequence of integers in the given range.
func _randomIntegers(in range: Range<Int>) -> UnfoldFirstSequence<Int> {
    return sequence(first: 0, next: { _ in Int.random(in: range) })
}

/// Random integers from nearly the full range of Ints; very few collisions
func randomLargeRange(_ size: Int) -> [Int] {
    return Array(_randomIntegers(in: .min ..< .max).prefix(size))
}

/// Random integers from a small range; lots of collisions
func randomSmallRange(_ size: Int) -> [Int] {
    let range = 0..<max(11, size / 100)
    return Array(_randomIntegers(in: range).prefix(size))
}

/// Many short in-order runs of integers
/// e.g. [0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, ...]
func smallRuns(_ size: Int) -> [Int] {
    let rangeBallpark = max(8, size / 100)
    var rangeCounts = _randomIntegers(in: rangeBallpark - 8 ..< rangeBallpark + 8)
    var result: [Int] = []
    while result.count < size {
        let c = min(size - result.count, rangeCounts.next()!)
        result += Array(0..<c)
    }
    return result
}

/// Long, repeating, in-order runs of integers
func largeRuns(_ size: Int) -> [Int] {
    let n = 11
    return Array((0..<n).map({ _ in 0..<(size / n) }).joined())
}

/// Integers ascending to the middle of the array, then descending to the end
func pyramid(_ size: Int) -> [Int] {
    let r = 0..<size / 2
    return Array(r) + r.reversed()
}

/// Integers descending to the middle of the array, then ascending to the end
func valley(_ size: Int) -> [Int] {
    let r = 0..<size / 2
    return r.reversed() + Array(r)
}

/// Returns an array generator that reverses the output of the given generator.
func reverse(_ g: @escaping (Int) -> [Int]) -> (Int) -> [Int] {
    return { g($0).reversed() }
}

/// Create a list of the generators, including reversed versions of the ones
/// for which that makes sense.
let generators: KeyValuePairs = [
    "InOrder": inOrder,
    "InOrderWithAppends": inOrderWithAppends,
    "InOrderWithPrepends": inOrderWithPrepends,
    "InOrderWithSprinkles": inOrderWithSprinkles,
    "SmallRuns": smallRuns,
    "LargeRuns": largeRuns,
    
    // These don't need to be reversed
    "RandomLargeRange": randomLargeRange,
    "RandomSmallRange": randomSmallRange,
    "Pyramid": pyramid,
    "Valley": valley,
]
let allGenerators = generators + generators.dropLast(4).map({ (name, generator) in
    (name + "Reversed", reverse(generator))
})

/// The identity transformation.
func identity<T>(_ x: T) -> T {
    return x
}

/// A struct that is larger than can be sorted as efficiently as an Int.
struct LargeComparable: Comparable {
    var value: Int
    var extra: (Int, Int, Int, Int, Int) = (0, 0, 0, 0, 0)
    
    init(_ value: Int) {
        self.value = value
    }
    
    static func == (lhs: LargeComparable, rhs: LargeComparable) -> Bool {
        return lhs.value == rhs.value
    }
    
    static func <(lhs: LargeComparable, rhs: LargeComparable) -> Bool {
        return lhs.value < rhs.value
    }
}

/// A reference type that boxes an Int.
final class IntBox: Comparable {
    var value: Int
    
    init(_ value: Int) {
        self.value = value
    }
    
    static func == (lhs: IntBox, rhs: IntBox) -> Bool {
        return lhs.value == rhs.value
    }
    
    static func <(lhs: IntBox, rhs: IntBox) -> Bool {
        return lhs.value < rhs.value
    }
}

extension Collection where Element: Comparable {
    /// Returns a Boolean value indicating whether the collection is sorted in
    /// ascending order.
    func isSorted() -> Bool {
        return zip(self, self.dropFirst()).allSatisfy(<=)
    }
}

/// Returns a benchmark using the given array generator and transformation.
///
/// - Parameters:
///   - arraySize: The size of the arrays to generate and sort. This can be
///     larger or smaller to produce the desired benchmark length based on
///     the kind of element produced by `transform`.
///   - generator: A closure that produces an array of integers of a specified
///     length.
///   - transform: An transforming closure that produces the kind of element
///     that this benchmark will test the speed of sorting.
func produceBenchmark<T: Comparable>(
    _ arraySize: Int,
    _ generator: @escaping (Int) -> [Int],
    _ transform: @escaping (Int) -> T
    ) -> (Int) -> Void {
    let array = generator(arraySize).map(transform)
    return { N in
        for _ in 0..<N {
            let sortedArray = array.sorted()
            CheckResults(sortedArray.isSorted())
        }
    }
}

public let Sorting =
    allGenerators.map({ (name, generator) in
        BenchmarkInfo(
            name: "Sorting\(name)",
            runFunction: produceBenchmark(10_000, generator, identity),
            tags: [.algorithm])
    })
    + allGenerators.map({ (name, generator) in
        BenchmarkInfo(
            name: "Sorting\(name)LargeStruct",
            runFunction: produceBenchmark(1000, generator, LargeComparable.init),
            tags: [.algorithm])
    })
    + allGenerators.map({ (name, generator) in
        BenchmarkInfo(
            name: "Sorting\(name)IntBox",
            runFunction: produceBenchmark(1000, generator, IntBox.init),
            tags: [.algorithm])
    })

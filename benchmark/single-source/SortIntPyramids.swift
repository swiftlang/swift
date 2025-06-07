import TestsUtils

// This benchmark aims to measure heapSort path of stdlib sorting function.
// Datasets in this benchmark are influenced by stdlib partition function,
// therefore if stdlib partition implementation changes we should correct these
// datasets or disable/skip this benchmark
public let benchmarks = [
  BenchmarkInfo(
    name: "SortIntPyramid",
    runFunction: run_SortIntPyramid,
    tags: [.validation, .api, .algorithm],
    legacyFactor: 5),
  BenchmarkInfo(
    name: "SortAdjacentIntPyramids",
    runFunction: run_SortAdjacentIntPyramids,
    tags: [.validation, .api, .algorithm],
    legacyFactor: 5),
]

// let A - array sorted in ascending order,
// A^R - reversed array A, + - array concatenation operator
// A indices are in range 1...A.length
// define the pyramid as A + A^R
// define pyramid height as A[A.length]

// On 92% of following dataset stdlib sorting function will use heapSort.
// number of ranges sorted by heapSort: 26
// median heapSort range length: 198
// maximum -||-: 1774
// average -||-: 357

// pyramid height
let pH = 5000
let pyramidTemplate: [Int] = (1...pH) + (1...pH).reversed()

// let A - array sorted in ascending order,
// A^R - reversed array A, + - array concatenation operator,
// A indices are in range 1...A.length.
// define adjacent pyramid as A + A^R + A + A^R,
// define adjacent pyramid height as A[A.length].


// On 25% of following dataset stdlib sorting function will use heapSort.
// number of ranges sorted by heapSort: 71
// median heapSort range length: 28
// maximum -||-: 120
// average -||-: 36

// adjacent pyramids height.
let aPH = pH / 2
let adjacentPyramidsTemplate: [Int] = (1...aPH) + (1...aPH).reversed()
                                    + (1...aPH) + (1...aPH).reversed()

@inline(never)
public func run_SortIntPyramid(_ n: Int) {
  for _ in 1...5*n {
    var pyramid = pyramidTemplate

    // sort pyramid in place.
    pyramid.sort()

    // Check whether pyramid is sorted.
    check(pyramid[0] <= pyramid[pyramid.count/2])
  }
}

@inline(never)
public func run_SortAdjacentIntPyramids(_ n: Int) {
  for _ in 1...5*n {
    var adjacentPyramids = adjacentPyramidsTemplate
    adjacentPyramids.sort()
    // Check whether pyramid is sorted.
    check(
      adjacentPyramids[0] <= adjacentPyramids[adjacentPyramids.count/2])
  }
}

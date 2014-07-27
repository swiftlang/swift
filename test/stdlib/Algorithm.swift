// RUN: %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import StdlibUnittest
import Swift

var Algorithm = TestCase("Algorithm")

typealias CodePoints = String.UnicodeScalarView

extension CodePoints {
  init(_ x: String) {
    self = x.unicodeScalars
  }
}

func print(x: CodePoints) { print(String(x._core)) }
func println(x: CodePoints) { println(String(x._core)) }

func println(x: [CodePoints]) {
  print("[ ")
  var prefix=""
  for s in x {
    print(prefix)
    print("\"")
    print(s)
    print("\"")
    prefix = ", "
  }
  println(" ]")
}

func testSplit() {
  // CHECK: testing split
  println("testing split")
  
  // CHECK-NEXT: [ "foo", "  bar baz " ]
  println(split(CodePoints("  foo   bar baz "), { $0._isSpace() }, maxSplit:1))

  // CHECK-NEXT: [ "foo", "bar", "baz" ]
  println(split(CodePoints("  foo   bar baz "), { $0._isSpace() }, allowEmptySlices: false))

  // CHECK-NEXT: [ "", "", "foo", "", "", "bar", "baz", "" ]
  println(split(CodePoints("  foo   bar baz "), { $0._isSpace() }, allowEmptySlices: true))

  // FIXME: Disabled pending <rdar://problem/15736729> and <rdar://problem/15733855>
  // CHECK-NEXT-DISABLED: [ "", "", "foo   bar baz " ]
  // println(split(CodePoints("  foo   bar baz "), { $0._isSpace() }, true, maxSplit:2))

  println("done.")
}
testSplit()

struct StartsWithTest {
  let expected: Bool
  let sequence: [Int]
  let prefix: [Int]
  let loc: SourceLoc

  init(
    _ expected: Bool, _ sequence: [Int], _ prefix: [Int],
    file: String = __FILE__, line: UWord = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.prefix = prefix
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

let startsWithTests = [
  StartsWithTest(true, [], []),
  StartsWithTest(false, [], [ 1 ]),
  StartsWithTest(true, [ 1 ], []),
  StartsWithTest(true, [ 0, 1, 3, 5 ], [ 0, 1 ]),
  StartsWithTest(true, [ 0, 1 ], [ 0, 1 ]),
  StartsWithTest(false, [ 0, 1, 3, 5 ], [ 0, 1, 4 ]),
  StartsWithTest(false, [ 0, 1 ], [ 0, 1, 4 ]),
]

func checkStartsWith(
  expected: Bool, sequence: [Int], prefix: [Int],
  stackTrace: SourceLocStack
) {
  expectEqual(expected, startsWith(sequence, prefix), stackTrace: stackTrace)
  expectEqual(
    expected, startsWith(sequence, prefix) { $0 == $1 },
    stackTrace: stackTrace)
  expectEqual(
    expected, startsWith(map(sequence) { $0 * 2 }, prefix) { $0 / 2 == $1 },
    stackTrace: stackTrace)

  // Test using different types for the sequence and prefix.
  expectEqual(
    expected, startsWith(ContiguousArray(sequence), prefix),
    stackTrace: stackTrace)
  expectEqual(
    expected, startsWith(ContiguousArray(sequence), prefix) { $0 == $1 },
    stackTrace: stackTrace)
}

Algorithm.test("startsWith") {
  for test in startsWithTests {
    checkStartsWith(
      test.expected, test.sequence, test.prefix, test.loc.withCurrentLoc())
  }
}

func testEnumerate() {
  println("testing enumerate")
  // CHECK: testing enumerate
  for (i, s) in enumerate( "You will never retrieve the necronomicon!"._split(" ") ) {
      println("\(i): \(s)")
  }
  // CHECK-NEXT: 0: You
  // CHECK-NEXT: 1: will
  // CHECK-NEXT: 2: never
  // CHECK-NEXT: 3: retrieve
  // CHECK-NEXT: 4: the
  // CHECK-NEXT: 5: necronomicon!
  println("done.")
}
testEnumerate()

func testEqual() {
  // CHECK: testing equal
  println("testing equal")
  var _0_4 = [0, 1, 2, 3]
  // CHECK-NEXT: false
  println(equal(_0_4, 0..<3))
  // CHECK-NEXT: true
  println(equal(_0_4, 0..<4))
  // CHECK-NEXT: false
  println(equal(_0_4, 0..<5))
  // CHECK-NEXT: false
  println(equal(_0_4, 1..<4))
  println("done.")
}
testEqual()

func testEqualPred() {
  // CHECK: testing equal with predicate
  println("testing equal with predicate")
  func compare(lhs: (Int, Int), rhs: (Int, Int)) -> Bool {
    return lhs.0 == rhs.0 && lhs.1 == rhs.1
  }
  var _0_4 = [(0, 10), (1, 11), (2, 12), (3, 13)]
  // CHECK-NEXT: false
  println(equal(_0_4, [(0, 10), (1, 11), (2, 12)], compare))
  // CHECK-NEXT: true
  println(equal(_0_4, [(0, 10), (1, 11), (2, 12), (3, 13)], compare))
  // CHECK-NEXT: false
  println(equal(_0_4, [(0, 10), (1, 11), (2, 12), (3, 13), (4, 14)], compare))
  // CHECK-NEXT: false
  println(equal(_0_4, [(1, 11), (2, 12), (3, 13)], compare))
  println("done.")
}
testEqualPred()

func testContains() {
  // CHECK: testing contains
  println("testing contains")
  let _0_4 = [0, 1, 2, 3]
  // CHECK-NEXT: false
  println(contains(_0_4, 7))
  // CHECK-NEXT: true
  println(contains(_0_4, 2))
  // CHECK-NEXT: false
  println(contains(_0_4, { $0 - 10 > 0  }))
  // CHECK-NEXT: true
  println(contains(_0_4, { $0 % 3 == 0 }))
  println("done.")
  
}
testContains()

func testMinMax() {
  // CHECK: testing min max
  println("testing min max")
  // CHECK-NEXT: 2
  println(min(3, 2))
  // CHECK-NEXT: 3
  println(min(3, 7, 5))
  // CHECK-NEXT: 3
  println(max(3, 2))
  // CHECK-NEXT: 7
  println(max(3, 7, 5))

  // FIXME: add tests that check that min/max return the
  // first element of the sequence (by reference equailty) that satisfy the
  // condition.
}

testMinMax()

Algorithm.test("minElement,maxElement") {
  var arr = [Int](count: 10, repeatedValue: 0)
  for i in 0..<10 {
    arr[i] = i % 7 + 2
  }
  expectEqual([2, 3, 4, 5, 6, 7, 8, 2, 3, 4], arr)

  expectEqual(2, minElement(arr))
  expectEqual(8, maxElement(arr))

  // min and max element of a slice
  expectEqual(3, minElement(arr[1..<5]))
  expectEqual(6, maxElement(arr[1..<5]))

  // FIXME: add tests that check that minElement/maxElement return the
  // first element of the sequence (by reference equailty) that satisfy the
  // condition.
}

func testFilter() {
  var count = 0
  
  // Make sure filter is eager and only calls its predicate once per
  // element.
  let one = filter(0..<10) {
    (x: Int)->Bool in ++count; return x == 1
  }
  // CHECK-NEXT: <10>
  for x in one {
    println("<\(count)>")
  }
  // CHECK-NEXT: <10>
  for x in one {
    println("<\(count)>")
  }
}
testFilter()

Algorithm.test("sorted") {
  expectEqual([ "Banana", "apple", "cherry" ],
      sorted([ "apple", "Banana", "cherry" ]))

  expectEqual([ "Banana", "cherry", "apple" ],
      sorted(["apple", "Banana", "cherry"]) {
        countElements($0) > countElements($1)
      })
}

// A wrapper around Array<T> that disables any type-specific algorithm
// optimizations and forces bounds checking on.
struct A<T> : MutableSliceable {
  init(_ a: Array<T>) {
    impl = a
  }

  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return impl.count
  }

  func generate() -> Array<T>.Generator {
    return impl.generate()
  }
  
  subscript(i: Int) -> T {
    get {
      expectTrue(i >= 0 && i < impl.count)
      return impl[i]
    }
    set (x) {
      expectTrue(i >= 0 && i < impl.count)
      impl[i] = x
    }
  }

  subscript(r: Range<Int>) -> Array<T>.SubSlice {
    get {
      expectTrue(r.startIndex >= 0 && r.startIndex <= impl.count)
      expectTrue(r.endIndex >= 0 && r.endIndex <= impl.count)
      return impl[r]
    }
    set (x) {
      expectTrue(r.startIndex >= 0 && r.startIndex <= impl.count)
      expectTrue(r.endIndex >= 0 && r.endIndex <= impl.count)
      impl[r] = x
    }
  }
  
  var impl: Array<T>
}

func withInvalidOrderings(body: ((Int,Int)->Bool)->Void) {
  // Test some ordering predicates that don't create strict weak orderings
  body { (_,_) in true }
  body { (_,_) in false }
  var i = 0
  body { (_,_) in i++ % 2 == 0 }
  body { (_,_) in i++ % 3 == 0 }
  body { (_,_) in i++ % 5 == 0 }
}

@asmname("random") func random() -> UInt32
@asmname("srandomdev") func srandomdev()

func randomArray() -> A<Int> {
  let count = random() % 50
  var a: [Int] = []
  a.reserveCapacity(Int(count))
  for i in 0..<count {
    a.append(Int(random()))
  }
  return A(a)
}

Algorithm.test("invalidOrderings") {
  srandomdev()
  withInvalidOrderings {
    var a = randomArray()
    sort(&a, $0)
  }
  withInvalidOrderings {
    var a: A<Int>
    a = randomArray()
    partition(&a, indices(a), $0)
  }
  /*
  // FIXME: Disabled due to <rdar://problem/17734737> Unimplemented:
  // abstraction difference in l-value
  withInvalidOrderings {
    var a = randomArray()
    var pred = $0
    _insertionSort(&a, indices(a), &pred)
  }
  */
  withInvalidOrderings {
    let predicate: (Int,Int)->Bool = $0
    let result = lexicographicalCompare(randomArray(), randomArray(), predicate)
  }
}

Algorithm.run()
// CHECK: {{^}}Algorithm: All tests passed

// CHECK-NEXT: all done.
println("all done.")


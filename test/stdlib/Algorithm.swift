// RUN: %target-build-swift -Xfrontend -disable-access-control %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import StdlibUnittest

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

//
// Convenience functions for testing
//

// FIXME: Until <rdar://problem/13985164> is fixed, we can't build
// generic algorithms that work on Sequences, so need a lightweight
// way to get Streams out of them.
prefix operator ^ {}
prefix func ^ (x: [Int]) -> Array<Int>.Generator
{ return x.generate() }

// FIXME: This class is a temporary workaround for
// <rdar://problem/13987068> (Vector enumerators can dangle)
struct VecIntStream : GeneratorType, SequenceType {
  typealias Element = Int

  init(_ owner: [Int]) {
    self.owner = owner
    self.value = owner.generate()
  }
  mutating
  func next() -> Element? { return value.next() }
  
  func generate() -> VecIntStream {
    return self
  }
  var owner: [Int]
  var value: Array<Int>.Generator
}

prefix operator ^^ {}
prefix func ^^ (x: [Int]) -> VecIntStream
{ 
  var result = Array<Int>()
  
  for a in x { result.append(a) }
  return VecIntStream(result)
}

func testStartsWith() {
  // CHECK: testing startsWith
  println("testing startsWith")
  
  // CHECK-NEXT: true
  println(startsWith(^[0, 1, 3, 5], ^[0, 1]))

  // CHECK-NEXT: true
  println(startsWith(^[0, 1], ^[0, 1]))

  // CHECK-NEXT: false
  println(startsWith(^[0, 1, 3, 5], ^[0, 1, 4]))

  // CHECK-NEXT: false
  println(startsWith(^[0, 1], ^[0, 1, 4]))


  // Same tests using heterogeneous enumerators
  // CHECK-NEXT: true
  println(startsWith(^^[0, 1, 3, 5], ^[0, 1]))

  // CHECK-NEXT: true
  println(startsWith(^^[0, 1], ^[0, 1]))

  // CHECK-NEXT: false
  println(startsWith(^^[0, 1, 3, 5], ^[0, 1, 4]))
  // CHECK-NEXT: false
  println(startsWith(^^[0, 1], ^[0, 1, 4]))
  println("done.")
}
testStartsWith()

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

Algorithm.run()
// CHECK: {{^}}Algorithm: All tests passed

// CHECK-NEXT: all done.
println("all done.")


// RUN: %target-run-simple-swift | FileCheck %s

typealias CodePoints = String.UnicodeScalarView

extension CodePoints {
  init(_ x: String) {
    self = x.unicodeScalars
  }
}

func print(x: CodePoints) { print(String(x._base)) }
func println(x: CodePoints) { println(String(x._base)) }

func println(x: CodePoints[]) {
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
  println(split(CodePoints("  foo   bar baz "), { $0.isSpace() }, maxSplit:1))

  // CHECK-NEXT: [ "foo", "bar", "baz" ]
  println(split(CodePoints("  foo   bar baz "), { $0.isSpace() }, allowEmptySlices: false))

  // CHECK-NEXT: [ "", "", "foo", "", "", "bar", "baz", "" ]
  println(split(CodePoints("  foo   bar baz "), { $0.isSpace() }, allowEmptySlices: true))

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
operator prefix ^ {}
@prefix func ^ (x: Int[]) -> Array<Int>.GeneratorType
{ return x.generate() }

// FIXME: This class is a temporary workaround for
// <rdar://problem/13987068> (Vector enumerators can dangle)
struct VecIntStream : Generator, Sequence {
  typealias Element = Int

  init(_ owner: Int[]) {
    self.owner = owner
    self.value = owner.generate()
  }
  mutating
  func next() -> Element? { return value.next() }
  
  func generate() -> VecIntStream {
    return self
  }
  var owner: Int[]
  var value: Array<Int>.GeneratorType
}

operator prefix ^^ {}
@prefix func ^^ (x: Int[]) -> VecIntStream
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
  for (i, s) in enumerate( "You will never retrieve the necronomicon!".split(" ") ) {
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
  println(equal(_0_4, 0..3))
  // CHECK-NEXT: true
  println(equal(_0_4, 0..4))
  // CHECK-NEXT: false
  println(equal(_0_4, 0..5))
  // CHECK-NEXT: false
  println(equal(_0_4, 1..4))
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

// RUN: %target-run-simple-swift(-enable-experimental-feature BorrowingForLoop \
// RUN: -Xfrontend -disable-availability-checking) \
// RUN:     %s | %FileCheck %s

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: executable_test

struct NoncopyableInt: ~Copyable {
  var value: Int
}

extension NoncopyableInt: Equatable {
  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
    lhs.value == rhs.value
  }
}

func testContinueTarget(seq: borrowing Span<NoncopyableInt>) {
  for element in seq {
    if (element.value == 2){
        continue
    }
    // CHECK: element.value = 0
    // CHECK: element.value = 1
    // CHECK: element.value = 3
    print("element.value = \(element.value)")
  }
}

func testBreakTarget(seq: borrowing Span<NoncopyableInt>) {
  for element in seq {
    if (element.value == 3){
        break
    }
    // CHECK: element.value = 0
    // CHECK: element.value = 1
    // CHECK: element.value = 2
    print("element.value = \(element.value)")
  }
}

func testWhereClause(seq: borrowing Span<NoncopyableInt>) {
  for element in seq where element.value.isMultiple(of: 2) {
    // CHECK: element.value = 0
    // CHECK: element.value = 2
    print("element.value = \(element.value)")
  }
}

func testWhereClauseWithContinue(seq: borrowing Span<NoncopyableInt>) {
  for element in seq where element.value > 0 {
    if (element.value == 2){
      continue
    }
    // CHECK: element.value = 1
    // CHECK: element.value = 3
    print("element.value = \(element.value)")
  }
}

func testWhereClauseWithBreak(seq: borrowing Span<NoncopyableInt>) {
  for element in seq where element.value.isMultiple(of: 2) {
    if (element.value > 1){
      break
    }
    // CHECK: element.value = 0
    print("element.value = \(element.value)")
  }
}

func testNestedForLoops(seq: borrowing Span<NoncopyableInt>) {
  for element in seq{
    let arr = [0, 1, 2, 3]
    let span = arr.span
    for x in span {
      if (x == element.value && x == 1){
        // CHECK: 1 == 1
        print("\(x) == \(element.value)")
      }
    }
  }
}

func testNestedForLoopsWithBreak(seq: borrowing Span<NoncopyableInt>) {
  for element in seq{
    let arr = [0, 1, 2, 3]
    let span = arr.span
    for x in span {
      if (x == element.value){
        // CHECK: 0 == 0
        print("\(x) == \(element.value)")
        break
      }
    }
  }
}

func testNestedForLoopsWithContinue(seq: borrowing Span<NoncopyableInt>) {
  for element in seq{
    let arr = [0, 1, 2, 3]
    let span = arr.span
    for x in span {
      if (x == 2){
        continue
      }
      if (x == element.value){
        // CHECK: 0 == 0
        // CHECK: 1 == 1
        // CHECK: 3 == 3
        print("\(x) == \(element.value)")
      }
    }
  }
}

func testNestedForLoopsWithWhereClause(seq: borrowing Span<NoncopyableInt>) {
  for element in seq where element.value != 2 {
    let arr = [0, 1, 2, 3]
    let span = arr.span
    for x in span {
      if (x == element.value){
        // CHECK: 0 == 0
        // CHECK: 1 == 1
        // CHECK: 3 == 3
        print("\(x) == \(element.value)")
      }
    }
  }
}

let buffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 4)
buffer.initializeElement(at: 0, to: NoncopyableInt(value: 0))
buffer.initializeElement(at: 1, to: NoncopyableInt(value: 1))
buffer.initializeElement(at: 2, to: NoncopyableInt(value: 2))
buffer.initializeElement(at: 3, to: NoncopyableInt(value: 3))

let seq = Span<NoncopyableInt>(_unsafeElements: buffer)

testContinueTarget(seq: seq)
testBreakTarget(seq: seq)
testWhereClause(seq: seq)
testWhereClauseWithContinue(seq: seq)
testWhereClauseWithBreak(seq: seq)
testNestedForLoops(seq: seq)
testNestedForLoopsWithBreak(seq: seq)
testNestedForLoopsWithContinue(seq: seq)
testNestedForLoopsWithWhereClause(seq: seq)

func testCopyableContinueTarget(seq: borrowing Span<Int>) {
  for element in seq {
    if (element == 2){
        continue
    }
    // CHECK: element = 0
    // CHECK: element = 1
    // CHECK: element = 3
    print("element = \(element)")
  }
}

func testCopyableBreakTarget(seq: borrowing Span<Int>) {
  for element in seq {
    if (element == 3){
        break
    }
    // CHECK: element = 0
    // CHECK: element = 1
    // CHECK: element = 2
    print("element = \(element)")
  }
}

func testCopyableWhereClause(seq: borrowing Span<Int>) {
  for element in seq where element.isMultiple(of: 2) {
    // CHECK: element = 0
    // CHECK: element = 2
    print("element = \(element)")
  }
}

func testCopyableWhereClauseWithContinue(seq: borrowing Span<Int>) {
  for element in seq where element > 0 {
    if (element == 2){
      continue
    }
    // CHECK: element = 1
    // CHECK: element = 3
    print("element = \(element)")
  }
}

func testCopyableWhereClauseWithBreak(seq: borrowing Span<Int>) {
  for element in seq where element.isMultiple(of: 2) {
    if (element > 1){
      break
    }
    // CHECK: element = 0
    print("element = \(element)")
  }
}

func testCopyableNestedForLoops(seq: borrowing Span<Int>) {
  for element in seq{
    let arr = [0, 1, 2, 3]
    let span = arr.span
    for x in span {
      if (x == element && x == 1){
        // CHECK: 1 == 1
        print("\(x) == \(element)")
      }
    }
  }
}

let arr = [0, 1, 2 , 3]
let copyableSpan = arr.span
testCopyableContinueTarget(seq: copyableSpan)
testCopyableBreakTarget(seq: copyableSpan)
testCopyableWhereClause(seq: copyableSpan)
testCopyableWhereClauseWithContinue(seq: copyableSpan)
testCopyableWhereClauseWithBreak(seq: copyableSpan)
testCopyableNestedForLoops(seq: copyableSpan)

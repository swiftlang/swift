// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

// CHECK: enum `an enum` {
enum `an enum` {

  // CHECK: case `c one`
  case `c one`

  // CHECK: case `default`(Int)
  case `default`(Int)

  // CHECK: case labeled(`p lbl`: Int, other: String)
  case labeled(`p lbl`: Int, other: String)
}

// CHECK: struct `a struct` {
struct `a struct` {
  // CHECK: var `some prop`: Int = 0
  var `some prop`: Int = 0

  // CHECK-LABEL: func `do thing`(`with arg` x: Int) -> Int {
  func `do thing`(`with arg` x: Int) -> Int { return x }
  //CHECK-NEXT:   return x
  //CHECK-NEXT: }

  // `self` and other keyword-named references must not be escaped.
  // CHECK-LABEL: internal func selfRef() -> `a struct` {
  func selfRef() -> `a struct` { return self }
  // CHECK-NEXT:   return self
  // CHECK-NEXT: }
}

// CHECK-LABEL: internal func testPatterns(_ e: `an enum`) -> Int {
func testPatterns(_ e: `an enum`) -> Int {
  switch e {
  case .`c one`:
    return 0
  case .`default`(let x) where x > 1:
    return x
  case .labeled(`p lbl`: let x, other: let y) where y.isEmpty:
    return x
  default:
    return 2
  }
}
// CHECK-NEXT:   switch e {
// CHECK-NEXT:   case .`c one`:
// CHECK-NEXT:     return 0
// CHECK-NEXT:   case .`default`(let x) where x > 1:
// CHECK-NEXT:     return x
// CHECK-NEXT:   case .labeled(`p lbl`: let x, other: let y) where y.isEmpty:
// CHECK-NEXT:     return x
// CHECK-NEXT:   default:
// CHECK-NEXT:     return 2
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK-LABEL: internal func testIfCase(_ e: `an enum`) -> Bool {
func testIfCase(_ e: `an enum`) -> Bool {
  if case .`default`(let x) = e {
    return x > 0
  }
  return false
}
// CHECK:        return x > 0

// CHECK-LABEL: internal func testExprs(_ s: `a struct`) -> Int {
func testExprs(_ s: `a struct`) -> Int {
  // CHECK: let `local var`: Int = s.`some prop`
  let `local var` = s.`some prop`
  
  // CHECK: return `local var` + s.`do thing`(`with arg`: 1)
  return `local var` + s.`do thing`(`with arg`: 1)
}

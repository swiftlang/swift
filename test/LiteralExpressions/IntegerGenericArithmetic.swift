// Literal expressions in integer generic parameter values
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip -verify

// =============================================================================
// Arithmetic operators
// =============================================================================

let add1: InlineArray<(2 + 3), Int> = [1, 2, 3, 4, 5]
let add2: InlineArray<(0 + 5), Int> = [1, 2, 3, 4, 5]
let add3: InlineArray<(1 + 1 + 1), Int> = [1, 2, 3]

let sub1: InlineArray<(5 - 2), Int> = [1, 2, 3]
let sub2: InlineArray<(10 - 5 - 2), Int> = [1, 2, 3]

let mul1: InlineArray<(2 * 3), Int> = [1, 2, 3, 4, 5, 6]
let mul3: InlineArray<(2 * 2 * 2), Int> = [1, 2, 3, 4, 5, 6, 7, 8]

let div1: InlineArray<(10 / 2), Int> = [1, 2, 3, 4, 5]
let div2: InlineArray<(9 / 3), Int> = [1, 2, 3]

let mod1: InlineArray<(7 % 4), Int> = [1, 2, 3]
let mod2: InlineArray<(10 % 3), Int> = [1]

// =============================================================================
// Bitwise operators
// =============================================================================

let and1: InlineArray<(7 & 3), Int> = [1, 2, 3]
let or1: InlineArray<(2 | 1), Int> = [1, 2, 3]
let xor1: InlineArray<(5 ^ 2), Int> = [1, 2, 3, 4, 5, 6, 7]

let lshift1: InlineArray<(1 << 3), Int> = [1, 2, 3, 4, 5, 6, 7, 8]
let lshift2: InlineArray<(1 << 5), Int> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                         11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                         21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                                         31, 32]

let rshift1: InlineArray<(16 >> 2), Int> = [1, 2, 3, 4]
let rshift2: InlineArray<(32 >> 3), Int> = [1, 2, 3, 4]

// =============================================================================
// Operator Precedence
// =============================================================================

let prec1: InlineArray<(2 + 3 * 2), Int> = [1, 2, 3, 4, 5, 6, 7, 8]
let prec2: InlineArray<(10 - 6 / 2), Int> = [1, 2, 3, 4, 5, 6, 7]
let prec3: InlineArray<(1 << 2 + 1), Int> = [1, 2, 3, 4, 5]

// =============================================================================
// Parentheses
// =============================================================================

let paren1: InlineArray<((2 + 3)), Int> = [1, 2, 3, 4, 5]
let paren2: InlineArray<((2 + 3) * 2), Int> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let paren3: InlineArray<(2 * (1 + 2)), Int> = [1, 2, 3, 4, 5, 6]
let paren4: InlineArray<((1 + 1) * 2), Int> = [1, 2, 3, 4]

// =============================================================================
// Sugar Syntax [N of T]
// =============================================================================

let sugar1: [(2 + 3) of Int] = [1, 2, 3, 4, 5]
let sugar2: [(1 << 2) of String] = ["a", "b", "c", "d"]
let sugar3: [((2 * 3)) of Double] = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

// =============================================================================
// Nested
// =============================================================================

let nested1: InlineArray<(1 + 1), InlineArray<(2 + 1), Int>> = [[1, 2, 3], [4, 5, 6]]
let nested2: [(2 * 2) of [(1 + 2) of Int]] = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]

// =============================================================================
// Function Parameters
// =============================================================================

func takeInlineArray<T>(_ arr: InlineArray<(2 + 2), T>) {}
func takeSugarArray<T>(_ arr: [(1 << 1) of T]) {}

func testFunctionCalls() {
  takeInlineArray([1, 2, 3, 4])
  takeInlineArray(["a", "b", "c", "d"])
  takeSugarArray([1, 2])
  takeSugarArray(["x", "y"])
}

// =============================================================================
// Type Aliases with Literal Expression Generics
// =============================================================================

typealias FourInts = InlineArray<(2 * 2), Int>
typealias EightStrings = InlineArray<(1 << 3), String>
typealias SugarSixDoubles = [(2 + 4) of Double]

let alias1: FourInts = [1, 2, 3, 4]
let alias2: EightStrings = ["a", "b", "c", "d", "e", "f", "g", "h"]
let alias3: SugarSixDoubles = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

// =============================================================================
// Struct Fields
// =============================================================================

struct Container {
    var data: InlineArray<(3 + 2), Int>
    var buffer: [(1 << 2) of UInt8]
}

func testStruct() {
  let c = Container(data: [1, 2, 3, 4, 5], buffer: [0, 1, 2, 3])
  _ = c
}

// =============================================================================
// Return Types
// =============================================================================

func makeArray() -> InlineArray<(2 + 1), Int> {
  return [1, 2, 3]
}

func makeSugarArray() -> [(4 - 1) of String] {
  return ["a", "b", "c"]
}

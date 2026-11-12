// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple

// Each row of a nested array literal with a known element type is solved as
// its own connected component, then the partial solutions are merged.

struct S {
  var a = 0
  var b = 1
}

func takesRows(_: [[Int]]) {}
func takesOptionalRows(_: [[Int]?]) {}

func contextual(flag: Bool) {
  let _: [[Int]] = [[0, 1], [2, 3]]
  let _: [[Int]] = [[0, 1], ([2, 3])]
  let _: [[Int]] = [([0, 1]), ((([2, 3])))]
  let _: [[Int]]? = [[0, 1], [2, 3]]
  let _: [[Int]?] = [[0, 1], nil, [2, 3]]
  let _: [[Int]?]? = [[0, 1], nil]
  let _: [[[Double]]] = [[[0, 1]], [[2.5]], []]
  let _: [[Any]] = [[0, "a"], [nil as Int? as Any]]
  takesRows([[0, 1], [2, 3]])
  takesOptionalRows([[0, 1], nil])

  let opaque: [[some Equatable]] = [[0, 1], [2, 3]]
  let opaqueRows: [[some Equatable]?] = [[0, 1], nil]
  _ = (opaque, opaqueRows)

  let _: [InlineArray<2, Int>] = [[0, 1], [2, 3]]
  let _: InlineArray<2, [Int]> = [[0], [1, 2]]
  let _: [2 of [2 of Int]] = [[0, 1], [2, 3]]
}

// Rows whose solving records new expression types (closure bodies, key paths)
// are merged back with those types intact.
func rowsSolvedIndependently(flag: Bool) {
  let closures: [[() -> Int]] = [[{ let x = 1; return x }], [{ 2 }, { [3].count }]]
  let _: [Int] = closures.map { $0.map { $0() }.reduce(0, +) }

  let _: [[KeyPath<S, Int>]] = [[\S.a], [\.b]]
  let _: [[Int]] = [[1 + 2, Int("3")!], [[4].count, { 5 }()]]
  let _: [[Int]] = [[flag ? 0 : 1], [flag ? 2 : 3]]
  let _: [[String]] = [["\(0)"], ["\(flag)", "b"]]
}

func assignment() {
  var dst: [[Int]] = []
  dst = [[0, 1], [2, 3]]
  dst = [[0, 1], ([2, 3])]

  var optionalRows: [[Int]?] = []
  optionalRows = [[0, 1], nil]

  var inlineRows: [2 of [Int]] = [[], []]
  inlineRows = [[0], [1, 2]]
  _ = (dst, optionalRows, inlineRows)
}

func inferred() {
  let a = [[0, 1], [2, 3]]
  let _: Int = a // expected-error {{cannot convert value of type '[[Int]]' to specified type 'Int'}}

  let b = [[0, 1], [2.5]]
  let _: Int = b // expected-error {{cannot convert value of type '[[Double]]' to specified type 'Int'}}

  let c = [[0], ["a"]]
  let _: Int = c // expected-error {{cannot convert value of type '[[Any]]' to specified type 'Int'}}
}

func illTyped() {
  let _: [[Int]] = [[0, "a"], [2, 3]] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
  let _: [[Int]] = [[0, 1], 2] // expected-error {{cannot convert value of type 'Int' to expected element type '[Int]'}}
  let _: [[Int]] = [[0, 1], ["a": 1]] // expected-error {{cannot convert value of type '[String : Int]' to expected element type '[Int]'}}
  let _: [[Int]?] = [[0, "a"], nil] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
  let _: InlineArray<2, [Int]> = [[0], [1], [2]] // expected-error {{expected 2 elements in inline array literal, but got 3}}
  let _: [2 of [2 of Int]] = [[0, 1], [2]] // expected-error {{cannot convert value of type '[Int]' to expected element type '[2 of Int]'}}

  var dst: [[Int]] = []
  dst = [[0, "a"]] // expected-error {{cannot convert value of type 'String' to expected element type 'Int'}}
  _ = dst

  takesRows([[0], [nil]]) // expected-error {{'nil' is not compatible with expected element type 'Int'}}
  takesRows([[0], [1, { "a" }()]]) // expected-error {{cannot convert value of type 'String' to closure result type 'Int'}}
}

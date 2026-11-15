// RUN: %target-swift-frontend -parse -verify %s

// Ordinary array types must not recursively rescan their element types while
// looking for the inline-array 'of' separator.
// https://github.com/swiftlang/swift/issues/94336
func nested(_: [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[Int]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]) {}

struct Box<T> {}

// Also exercise collection-type lookahead while disambiguating generic
// arguments from comparison operators.
let _ = Box<[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[Int]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.self

func collectionTypes(
  _: [[Int]: [String: Int]],
  _: [2 of [3 of Int]],
  _: [[2 of Int]],
  _: [(Int, String)],
  _: [() -> Int]
) {}

let _ = Box<[2 of [3 of Int]]>.self
let _ = Box<[(Int, String)]>.self
let _ = Box<[() -> Int]>.self

// Pack expansions remain ordinary type productions, not inline-array counts.
func packs<each T>(_: [repeat each T], _: [(repeat each T)]) {}

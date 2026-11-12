// RUN: %target-typecheck-verify-swift
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Generic arguments of result builder attributes that are omitted or written
// as placeholders are inferred from the body when the result builder transform
// is applied.

@resultBuilder
enum ArrayBuilder<Element> {
  static func buildBlock(_ components: [Element]...) -> [Element] {
    components.flatMap { $0 }
  }

  static func buildExpression(_ expression: Element) -> [Element] {
    [expression]
  }

  static func buildOptional(_ component: [Element]?) -> [Element] {
    component ?? []
  }

  static func buildEither(first component: [Element]) -> [Element] {
    component
  }

  static func buildEither(second component: [Element]) -> [Element] {
    component
  }

  static func buildArray(_ components: [[Element]]) -> [Element] {
    components.flatMap { $0 }
  }
}

@resultBuilder
enum CollectionBuilder<Element> {
  static func buildBlock(_ component: Element...) -> [Element] {
    component
  }

  static func buildFinalResult(_ component: [Element]) -> [Element] {
    component
  }

  static func buildFinalResult(_ component: [Element]) -> Set<Element> where Element: Hashable {
    Set(component)
  }
}

@resultBuilder
enum DictionaryBuilder<Key: Hashable, Value> {
  static func buildBlock(_ component: (key: Key, value: Value)...) -> [Key: [Value]] {
    var dictionary = [Key: [Value]]()
    for (key, value) in component {
      dictionary[key, default: []].append(value)
    }
    return dictionary
  }
}

extension Array {
  @resultBuilder
  enum Builder {
    static func buildBlock(_ elements: Element...) -> [Element] {
      elements
    }
  }

  init(@ArrayBuilder _ build: () -> Self) {
    self = build()
  }
}

@resultBuilder
enum OptionalArrayBuilder<Element> {
  static func buildBlock(_ elements: Element...) -> [Element] {
    elements
  }

  static func buildFinalResult(_ elements: [Element]) -> [Element] {
    print("[Element] overload")
    return elements
  }

  static func buildFinalResult(_ elements: [Element]) -> [Element?] {
    print("[Element?] overload")
    return elements.map { $0 } + [nil]
  }
}

// The same overloads as `OptionalArrayBuilder`, declared in the opposite
// order. Which overload is selected is determined by type inference on the
// transformed body, so doesn't depend on declaration order.
@resultBuilder
enum ReversedOptionalArrayBuilder<Element> {
  static func buildBlock(_ elements: Element...) -> [Element] {
    elements
  }

  static func buildFinalResult(_ elements: [Element]) -> [Element?] {
    print("reversed [Element?] overload")
    return elements.map { $0 } + [nil]
  }

  static func buildFinalResult(_ elements: [Element]) -> [Element] {
    print("reversed [Element] overload")
    return elements
  }
}

@resultBuilder
enum PairBuilder<Element> {
  static func buildBlock(_ elements: Element...) -> [Element] {
    elements
  }

  static func buildFinalResult(_ elements: [Element]) -> (Element, Double) {
    print("(Element, Double) overload")
    return (elements[0], 0)
  }

  static func buildFinalResult(_ elements: [Element]) -> (Int, Element) {
    print("(Int, Element) overload")
    return (0, elements[0])
  }
}

let recursive = true

@ArrayBuilder
var arguments: [String] {
  "format"
  "--in-place"

  if recursive {
    "--recursive"
  }

  for path in ["a.swift", "b.swift"] {
    path
  }
}

@ArrayBuilder
func makeArguments(verbose: Bool) -> [String] {
  "lint"
  if verbose {
    "--verbose"
  } else {
    "--quiet"
  }
}

@ArrayBuilder
func makeGenericArray<T>(_ first: T, _ second: T) -> [T] {
  first
  second
}

struct SwiftFormatInvocation {
  @ArrayBuilder let arguments: [String]
}

@CollectionBuilder
var array: [String] {
  "a"
  "b"
}

@CollectionBuilder
var set: Set<String> {
  "c"
  "d"
}

@DictionaryBuilder
var dictionary: [String: [Int]] {
  (key: "a", value: 42)
  (key: "b", value: 100)
}

@DictionaryBuilder<String, _>
var dictionaryWithPlaceholder: [String: [(a: Int, b: Int)]] {
  (key: "a", value: (a: 1, b: 2))
}

@Array.Builder
var nestedBuilder: [String] {
  "a"
  "b"
}

@Array<_>.Builder
var nestedBuilderWithPlaceholder: [Int] {
  1
  2
}

// Explicit generic arguments select different `buildFinalResult` overloads.
@OptionalArrayBuilder<Int>
var optionalValues1: [Int?] {
  1
  2
}

@OptionalArrayBuilder<Int?>
var optionalValues2: [Int?] {
  1
  2
}

// When the generic arguments are inferred, the overload is selected using the
// standard type inference rules applied to the transformed body:
//
//   OptionalArrayBuilder.buildFinalResult(OptionalArrayBuilder.buildBlock(1, 2))
//
// The integer literals prefer `Int`, so this is `@OptionalArrayBuilder<Int>`
// and uses the `[Element?]` overload, even though the `[Element]` overload is
// declared first.
@OptionalArrayBuilder
var optionalValues3: [Int?] {
  1
  2
}

// Declaration order doesn't matter.
@ReversedOptionalArrayBuilder
var reversedOptionalValues: [Int?] {
  1
  2
}

// Elements of type `Int?` infer `@OptionalArrayBuilder<Int?>` instead, which
// uses the `[Element]` overload.
@OptionalArrayBuilder
var optionalElements: [Int?] {
  Optional(1)
  Optional(2)
}

@OptionalArrayBuilder
var optionalElementCoercion: [Int?] {
  1 as Int?
  2
}

// Functions and closure parameters behave the same as computed properties.
@OptionalArrayBuilder
func makeOptionalValues() -> [Int?] {
  1
  2
}

func optionalValues(@OptionalArrayBuilder _ build: () -> [Int?]) -> [Int?] {
  build()
}

func reversedOptionalValues(
  @ReversedOptionalArrayBuilder _ build: () -> [Int?]
) -> [Int?] {
  build()
}

// `(Int, Double)` could be produced by `@PairBuilder<Int>` using the
// `(Element, Double)` overload, or `@PairBuilder<Double>` using the
// `(Int, Element)` overload. The elements in the body determine which.
@PairBuilder
var intPair: (Int, Double) {
  1
  2
}

@PairBuilder
var doublePair: (Int, Double) {
  1.5
  2.5
}

// CHECK: ["format", "--in-place", "--recursive", "a.swift", "b.swift"]
print(arguments)

// CHECK: ["lint", "--verbose"]
print(makeArguments(verbose: true))

// CHECK: ["lint", "--quiet"]
print(makeArguments(verbose: false))

// CHECK: [1, 2]
print(makeGenericArray(1, 2))

// CHECK: ["x", "y"]
print(Array { "x"; "y" } as [String])

// CHECK: ["format", "--recursive"]
print(SwiftFormatInvocation(arguments: { "format"; "--recursive" }).arguments)

// CHECK: ["a", "b"]
print(array)

// CHECK: ["c", "d"]
print(set.sorted())

// CHECK: [(key: "a", value: [42]), (key: "b", value: [100])]
print(dictionary.sorted { $0.key < $1.key })

// CHECK: [(a: 1, b: 2)]
print(dictionaryWithPlaceholder["a"]!)

// CHECK: ["a", "b"]
print(nestedBuilder)

// CHECK: [1, 2]
print(nestedBuilderWithPlaceholder)

// CHECK: [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(optionalValues1)

// CHECK-NEXT: [Element] overload
// CHECK-NEXT: [Optional(1), Optional(2)]
print(optionalValues2)

// CHECK-NEXT: [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(optionalValues3)

// CHECK-NEXT: reversed [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(reversedOptionalValues)

// CHECK-NEXT: [Element] overload
// CHECK-NEXT: [Optional(1), Optional(2)]
print(optionalElements)

// CHECK-NEXT: [Element] overload
// CHECK-NEXT: [Optional(1), Optional(2)]
print(optionalElementCoercion)

// CHECK-NEXT: [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(makeOptionalValues())

// CHECK-NEXT: [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(optionalValues {
  1
  2
})

// CHECK-NEXT: reversed [Element?] overload
// CHECK-NEXT: [Optional(1), Optional(2), nil]
print(reversedOptionalValues {
  1
  2
})

// CHECK-NEXT: [Element] overload
// CHECK-NEXT: [Optional(1), Optional(2)]
print(optionalValues {
  Optional(1)
  Optional(2)
})

// CHECK-NEXT: (Element, Double) overload
// CHECK-NEXT: (1, 0.0)
print(intPair)

// CHECK-NEXT: (Int, Element) overload
// CHECK-NEXT: (0, 1.5)
print(doublePair)

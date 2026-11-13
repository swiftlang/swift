// RUN: %target-typecheck-verify-swift
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Generic arguments of result builder attributes that are omitted or written
// as placeholders are inferred from the result type of the attached
// declaration, using standard type inference for an unapplied reference to the
// builder method that produces the final result, e.g.
//
//   let _: (_) -> Result = Builder.buildFinalResult
//
// If the result type doesn't determine them, they must be written explicitly.

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
// order. Overload resolution doesn't depend on declaration order.
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
enum ReportingBuilder<Element> {
  static func buildExpression(_ element: Element) -> [Element] {
    [element]
  }

  static func buildBlock(_ components: [Element]...) -> [Element] {
    print("ReportingBuilder<\(Element.self)>")
    return components.flatMap { $0 }
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

// When the generic arguments are inferred, they're the same as for
//
//   let _: (_) -> [Int?] = OptionalArrayBuilder.buildFinalResult
//
// which is `OptionalArrayBuilder<Int>`, using the `[Element?]` overload, even
// though the `[Element]` overload is declared first.
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

// The result type determines `Element`, even when it differs from the type of
// the first element.
@ReportingBuilder
var stringProtocols: [any StringProtocol] {
  "format"
  "--in-place".dropFirst(2)
}

@ReportingBuilder
var collections: [any Collection<String>] {
  ["format"]
  Set(["--in-place"])
}

@ReportingBuilder
var doubles: [Double] {
  1
}

@ReportingBuilder
var optionalStrings: [String?] {
  "format"
  nil
}

// `String` would also be valid here, but standard type inference picks
// `String?`, like `let _: (_...) -> [String?] = ReportingBuilder.buildBlock`.
@ReportingBuilder
var nonNilOptionalStrings: [String?] {
  "format"
  "--in-place"
}

// An opaque result type's requirements determine `Element`, like
// `let _: some Collection<String> = ReportingBuilder.buildBlock()`.
@ReportingBuilder
var opaqueStrings: some Collection<String> {
  "format"
  "--in-place"
}

struct Subscripted {
  @ReportingBuilder
  subscript(index: Int) -> [String] {
    "a"
    "b"
  }
}

protocol HasItems {
  @ReportingBuilder var items: [String] { get }
}

// Inherits `@ReportingBuilder` from the protocol requirement.
struct Items: HasItems {
  var items: [String] {
    "x"
    "y"
  }
}

// Generic contexts: inferred as `ReportingBuilder<Result>` and
// `ReportingBuilder<Arg>`, and then specialized at each use.
func build<Result>(@ReportingBuilder _ body: () -> [Result]) -> [Result] {
  body()
}

struct GenericInvocation<Arg> {
  @ReportingBuilder let args: [Arg]
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

// CHECK-NEXT: ReportingBuilder<StringProtocol>
// CHECK-NEXT: ["format", "in-place"]
print(stringProtocols.map { String($0) })

// CHECK-NEXT: ReportingBuilder<any Collection<Self.Collection.Element == String>>
// CHECK-NEXT: 2
print(collections.count)

// CHECK-NEXT: ReportingBuilder<Double>
// CHECK-NEXT: [1.0]
print(doubles)

// CHECK-NEXT: ReportingBuilder<Optional<String>>
// CHECK-NEXT: [Optional("format"), nil]
print(optionalStrings)

// CHECK-NEXT: ReportingBuilder<Optional<String>>
// CHECK-NEXT: [Optional("format"), Optional("--in-place")]
print(nonNilOptionalStrings)

// CHECK-NEXT: ReportingBuilder<String>
// CHECK-NEXT: ["format", "--in-place"]
print(Array(opaqueStrings))

// CHECK-NEXT: ReportingBuilder<String>
// CHECK-NEXT: ["a", "b"]
print(Subscripted()[0])

// CHECK-NEXT: ReportingBuilder<String>
// CHECK-NEXT: ["x", "y"]
print(Items().items)

// CHECK-NEXT: ReportingBuilder<String>
// CHECK-NEXT: ["format", "--in-place"]
print(build { "format"; "--in-place" })

// CHECK-NEXT: ReportingBuilder<Int>
// CHECK-NEXT: [1, 2]
print(build { 1; 2 })

// CHECK-NEXT: ReportingBuilder<String>
// CHECK-NEXT: ["format", "--recursive"]
print(GenericInvocation(args: { "format"; "--recursive" }).args)

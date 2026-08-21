// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// REQUIRES: executable_test

// Runtime coverage for the concrete-typed representation of single-payload
// enums whose payload holds a managed reference in a multi-word value: a
// class-bound existential optional, a struct mixing a reference and an integer,
// and a custom enum over an existential with extra empty cases. Construction,
// switching, and retain/release must all stay correct even though the enum now
// carries its pointer words as `ptr`.

protocol P: AnyObject {
  var n: Int { get }
}

final class Box: P {
  let n: Int
  init(_ n: Int) { self.n = n }
  deinit { print("deinit \(n)") }
}

// A class-bound existential optional: `(any P)?` is two pointers.
func describe(_ v: (any P)?) -> String {
  switch v {
  case .some(let p): return "some(\(p.n))"
  case .none: return "none"
  }
}

func testExistentialOptional() {
  print("== testExistentialOptional ==")  // CHECK: == testExistentialOptional ==
  do {
    let v: (any P)? = Box(1)
    print(describe(v))                     // CHECK-NEXT: some(1)
    print(describe(nil))                   // CHECK-NEXT: none
    let copy = v                           // retain through both words
    print(describe(copy))                  // CHECK-NEXT: some(1)
  }                                        // CHECK-NEXT: deinit 1
}
testExistentialOptional()

// A struct mixing a reference and an integer, wrapped in an optional.
struct Pair { var a: Box; var b: Int }

func testPairOptional() {
  print("== testPairOptional ==")          // CHECK: == testPairOptional ==
  do {
    let p: Pair? = Pair(a: Box(2), b: 42)
    switch p {
    case .some(let x): print("some(\(x.a.n), \(x.b))")  // CHECK-NEXT: some(2, 42)
    case .none: print("none")
    }
    print(p == nil ? "nil" : "nonnil")      // CHECK-NEXT: nonnil
  }                                         // CHECK-NEXT: deinit 2
}
testPairOptional()

// A padded payload: the leading Int32 pushes the reference into the second
// word, so the reference word is `ptr` while the padding stays in the leading
// integer word.
struct PaddedRef { var x: Int32; var ref: Box }

func testPaddedOptional() {
  print("== testPaddedOptional ==")          // CHECK: == testPaddedOptional ==
  do {
    let p: PaddedRef? = PaddedRef(x: 7, ref: Box(5))
    switch p {
    case .some(let v): print("some(\(v.x), \(v.ref.n))")  // CHECK-NEXT: some(7, 5)
    case .none: print("none")
    }
    print(p == nil ? "nil" : "nonnil")        // CHECK-NEXT: nonnil
  }                                           // CHECK-NEXT: deinit 5
}
testPaddedOptional()

// A custom single-payload enum over an existential with extra empty cases.
enum ExistentialOrTags {
  case value(any P)
  case a
  case b
}

func describe(_ e: ExistentialOrTags) -> String {
  switch e {
  case .value(let p): return "value(\(p.n))"
  case .a: return "a"
  case .b: return "b"
  }
}

func testExistentialOrTags() {
  print("== testExistentialOrTags ==")     // CHECK: == testExistentialOrTags ==
  print(describe(.a))                       // CHECK-NEXT: a
  print(describe(.b))                       // CHECK-NEXT: b
  do {
    let e: ExistentialOrTags = .value(Box(3))
    print(describe(e))                      // CHECK-NEXT: value(3)
  }                                         // CHECK-NEXT: deinit 3
}
testExistentialOrTags()

print("== done ==")                         // CHECK: == done ==

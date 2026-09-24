// RUN: %target-run-simple-swift(-Onone) | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// REQUIRES: executable_test

// Runtime coverage for the `ptr`-lowered single-payload enum representation:
// nested optionals of a class reference, and custom single-payload enums with
// several no-payload cases. These carry the empty cases in the payload
// pointer's low, invalid-pointer extra inhabitants; only genuine pointers are
// ever dereferenced, so construction, switching, and retain/release must all
// stay correct.

final class Box {
  let n: Int
  init(_ n: Int) { self.n = n }
  deinit { print("deinit \(n)") }
}

// A custom single-payload enum with a single retainable-pointer payload and
// more than one no-payload case.
enum Many {
  case value(Box)
  case a
  case b
  case c
}

func describe(_ e: Many) -> String {
  switch e {
  case .value(let b): return "value(\(b.n))"
  case .a: return "a"
  case .b: return "b"
  case .c: return "c"
  }
}

func testMany() {
  print("== testMany ==")           // CHECK: == testMany ==
  do {
    let e: Many = .value(Box(1))
    print(describe(e))              // CHECK-NEXT: value(1)
    print(describe(.a))             // CHECK-NEXT: a
    print(describe(.b))             // CHECK-NEXT: b
    print(describe(.c))             // CHECK-NEXT: c
    let copy = e                    // retain the payload
    print(describe(copy))           // CHECK-NEXT: value(1)
  }                                 // CHECK-NEXT: deinit 1
}
testMany()

// Nested optionals of a class reference.
func d2(_ v: Box??) -> String {
  switch v {
  case .some(.some(let b)): return "some(some(\(b.n)))"
  case .some(.none): return "some(none)"
  case .none: return "none"
  }
}

func testNested() {
  print("== testNested ==")         // CHECK: == testNested ==
  do {
    let x: Box?? = Box(2)
    print(d2(x))                    // CHECK-NEXT: some(some(2))
    print(d2(.some(.none)))         // CHECK-NEXT: some(none)
    print(d2(.none))                // CHECK-NEXT: none
    let copy = x                    // retain through two optional layers
    print(d2(copy))                 // CHECK-NEXT: some(some(2))
  }                                 // CHECK-NEXT: deinit 2

  do {
    let y: Box??? = Box(3)
    if case .some(.some(.some(let b))) = y {
      print("sss(\(b.n))")          // CHECK-NEXT: sss(3)
    }
  }                                 // CHECK-NEXT: deinit 3
}
testNested()

// A custom single-payload enum whose payload is itself a `ptr`-lowered optional.
enum OptPayload {
  case value(Box?)
  case p
  case q
}

func d3(_ f: OptPayload) -> String {
  switch f {
  case .value(.some(let b)): return "value(some(\(b.n)))"
  case .value(.none): return "value(none)"
  case .p: return "p"
  case .q: return "q"
  }
}

func testOptPayload() {
  print("== testOptPayload ==")     // CHECK: == testOptPayload ==
  print(d3(.value(nil)))            // CHECK-NEXT: value(none)
  print(d3(.p))                     // CHECK-NEXT: p
  print(d3(.q))                     // CHECK-NEXT: q
  do {
    let f: OptPayload = .value(Box(4))
    print(d3(f))                    // CHECK-NEXT: value(some(4))
  }                                 // CHECK-NEXT: deinit 4
}
testOptPayload()

print("== done ==")                 // CHECK: == done ==

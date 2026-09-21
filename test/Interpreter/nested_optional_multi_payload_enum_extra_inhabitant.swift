// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Regression test for rdar://179973126 / https://github.com/swiftlang/swift/issues/90031
//
// A doubly-nested optional whose innermost payload is a struct built on a
// multi-payload enum can consume the payload's extra inhabitants until the
// outer optional has zero residual extra inhabitants of its own. IRGen used to
// pick a whole-value comparison for the outer `.none` test in that situation
// (SinglePayloadEnumImplStrategy::emitValueCaseTest gated the comparison mask on
// the *enclosing* enum's residual extra-inhabitant count instead of the
// *payload's*). That over-constrained the test to require the payload's
// non-discriminator bits to be zero. A `.none` produced by the runtime value
// witness only sets the spare bits and leaves the rest arbitrary, so it was
// misread as `.some`, yielding a phantom payload that crashed reflection when
// printed.
//
// The specific case counts below are what push the outer optional's residual
// extra-inhabitant count down to exactly zero on 64-bit targets.

enum B: Hashable, Codable {
  case c(C)
  case d(D)
}

enum C: String, Codable {
  case _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17
  case _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, _33
}

enum D: String, CaseIterable, Codable {
  case _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12
}

struct ID: Hashable, Codable {
  let name: B
  let instance: Int
}

struct Wrapper: Codable {
  let a: ID?
}

func takeGenericOptional<T: Codable>(_ body: @escaping (_ data: inout T?) -> Void) {
  // `data` is default-initialized to `.none` by the runtime value witness for
  // `Optional<T>`, which only sets the spare bits.
  var data: T?
  body(&data)
}

// CHECK: data == nil: true
// CHECK-NEXT: nil
// CHECK-NEXT: nil
takeGenericOptional { (data: inout Wrapper?) in
  print("data == nil:", data == nil)
  print(data as Any)
  print(data?.a as Any) // used to crash in reflection via print
}

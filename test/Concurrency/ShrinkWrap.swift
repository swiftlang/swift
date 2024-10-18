// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -O %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://124277662
// XFAIL: freestanding

// CHECK: Setting field Field(rawValue: "fxxxxxxx/0")
// CHECK: Setting field Field(rawValue: "fxxxxxxx/1")
// CHECK: Setting field Field(rawValue: "fxxxxxxx/2")
// CHECK: Setting field Field(rawValue: "fxxxxxxx/3")

internal struct Field: Equatable, Hashable, Sendable {
  internal var rawValue: String

  internal static var c0: Field { Field(rawValue: "fxxxxxxx/0") }
  internal static var c1: Field { Field(rawValue: "fxxxxxxx/1") }
  internal static var c2: Field { Field(rawValue: "fxxxxxxx/2") }
  internal static var c3: Field { Field(rawValue: "fxxxxxxx/3") }

  internal static func c(_ c: Int) -> Field {
    switch c {
    case 0: return self.c0
    case 1: return self.c1
    case 2: return self.c2
    case 3: return self.c3
    default:
      preconditionFailure("Only 0..<4 are valid")
    }
  }
}

internal actor A {
  @inline(never)
  internal func set(_ field: Field) async throws {
    print("Setting field \(field)")
  }
}

func doit() async throws {
  let a = A()
  for c in 0..<Int(4) {
    try await Task.sleep(nanoseconds: 50_000_000)
    try await a.set(.c(c))
  }
}

try await doit()

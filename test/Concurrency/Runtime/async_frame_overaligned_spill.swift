// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding
// UNSUPPORTED: back_deployment_runtime

// A value live across a suspension whose ABI alignment is greater than the
// alignment the async context guarantees, here a SIMD3<Double> with an ABI
// alignment of 32 in a context aligned to 16, used to be laid out at one offset
// and emitted at another. The frame then ran past the context allocated for it
// and corrupted the task allocator, either tripping its stack discipline check
// or clobbering a refcounted slot.
//
// The aggregate parameter, the isolated parameter with a non foldable argument,
// and throws on both functions are all needed to get the vector spilled at an
// offset that is 16 but not 32 byte aligned.
//
// rdar://185503097, https://github.com/swiftlang/swift/issues/91639

actor Iso {}
let iso = Iso()

func withString(_ a: (String, SIMD3<Double>)) async throws {
  func local(_ a: (String, SIMD3<Double>), _: isolated (any Actor)? = iso) async throws {
    precondition(!a.0.isEmpty)
  }
  try await local(a)
}

final class Ref {
  let value: Int
  init(value: Int) { self.value = value }
}

func withClass(_ a: (Ref, SIMD3<Double>)) async throws {
  func local(_ a: (Ref, SIMD3<Double>), _: isolated (any Actor)? = iso) async throws {
    precondition(a.0.value == 42)
  }
  try await local(a)
}

func withArray(_ a: ([Int], SIMD3<Double>)) async throws {
  func local(_ a: ([Int], SIMD3<Double>), _: isolated (any Actor)? = iso) async throws {
    precondition(a.0.count == 3)
  }
  try await local(a)
}

@main struct Main {
  static func main() async throws {
    try await withString(("+X", SIMD3(1, 0, 0)))
    // CHECK: string ok
    print("string ok")

    try await withClass((Ref(value: 42), SIMD3(0, 1, 0)))
    // CHECK: class ok
    print("class ok")

    try await withArray(([1, 2, 3], SIMD3(0, 0, 1)))
    // CHECK: array ok
    print("array ok")
  }
}

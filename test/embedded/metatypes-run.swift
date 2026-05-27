// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func compare (
  _ t0: (any (~Copyable & ~Escapable).Type)?,
  _ t1: (any (~Copyable & ~Escapable).Type)?
) -> Bool {
  switch (t0, t1) {
  case (.none, .none):
    return true
  case let (.some(ty0), .some(ty1)):
    let p1 = unsafeBitCast(ty0, to: UnsafeRawPointer.self)
    let p2 = unsafeBitCast(ty1, to: UnsafeRawPointer.self)
    return p1 == p2
  default:
    return false
  }
}

@main
struct Main {
  static func main() {
    // CHECK: Starting...
    print("Starting...")
    precondition(compare(Void.self, Void.self))
    precondition(compare((Int, Float).self, (Int, Float).self))
    precondition(!compare((Float).self, (Int, Float).self))
    precondition(!compare((Float).Type.self, (Int, Float).self))
    precondition(compare((any Error).Type.self, (any Error).Type.self))
    // CHECK: Done!
    print("Done!")
  }
}

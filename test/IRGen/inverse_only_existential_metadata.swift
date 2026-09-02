// RUN: %target-swift-frontend -emit-ir -module-name test %s | %FileCheck %s

// Regression test for https://github.com/swiftlang/swift/issues/89402.
//
// An existential carrying ONLY inverse requirements (no parameterized
// protocols) must NOT lower through swift_getExtendedExistentialTypeMetadata.
// That runtime entry-point is unavailable in libswiftCore shipped with
// iOS 15 / macOS 12 and earlier; using it crashes with a NULL
// weak-external call. Inverse requirements alone are representable in
// the classical existential shape and should keep going through
// swift_getExistentialTypeMetadata.

// CHECK-NOT: call {{.*}}@swift_getExtendedExistentialTypeMetadata

public class Repro {
  public func getString<T>(str: String) -> T? {
    return nil
  }

  public func test() {
    // T has no contextual type. With PR #81365's behavior, IRGen would
    // infer `any Any<~Copyable, ~Escapable>` and lower the metadata
    // access through the extended path; the fix keeps it on the
    // classical path.
    if getString(str: "") != nil { return }
  }
}

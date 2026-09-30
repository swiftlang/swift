// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Onone -emit-sil -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// Embedded Swift specializes generics even at -Onone. Specializing a `throws(E)` function for
// `E == Never` leaves a rethrow which can never execute, and IRGen has to emit that as an
// unconditional trap.

public func mapped(_ o: Int?) -> Int? {
  o.map { $0 &+ 1 }
}

public func temporary(_ n: Int) -> Int {
  withUnsafeTemporaryAllocation(of: Int.self, capacity: n) { $0.count }
}

// CHECK-NOT: {{^ +throw}}

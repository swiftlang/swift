// RUN: %target-swift-frontend -O -emit-ir -parse-as-library %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib
// REQUIRES: CPU=arm64

// Optional<Int> (switch_enum) vs. a hand-rolled {Int, Bool} (cond_br).
// IRGen must not emit extra blocks for the switch_enum payload case, 
// ensure both loops get vectorized.

public struct ManualOpt {
  public var value: Int
  public var isSome: Bool
}

// CHECK-LABEL: define {{.*}}swiftcc i64 @"$s30switch_enum_optional_vectorize11sumOptionalySiSRySiSgGF"
// CHECK:       vector.body:
// CHECK:         call i64 @llvm.vector.reduce.add
@inline(never)
public func sumOptional(_ a: UnsafeBufferPointer<Int?>) -> Int {
  var s = 0
  for x in a {
    switch x {
    case .some(let v): s &+= v
    case .none: s &+= 7
    }
  }
  return s
}

// CHECK-LABEL: define {{.*}}swiftcc i64 @"$s30switch_enum_optional_vectorize9sumManualySiSRyAA0F3OptVGF"
// CHECK:       vector.body:
// CHECK:         call i64 @llvm.vector.reduce.add
@inline(never)
public func sumManual(_ a: UnsafeBufferPointer<ManualOpt>) -> Int {
  var s = 0
  for x in a {
    if x.isSome { s &+= x.value } else { s &+= 7 }
  }
  return s
}


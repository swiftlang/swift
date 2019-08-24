// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -sil-verify-all -module-name=test -emit-sil -enforce-exclusivity=unchecked | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -Osize -sil-verify-all -module-name=test -emit-sil -enforce-exclusivity=unchecked | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public struct TestOptions: OptionSet {
    public let rawValue: Int
    public init(rawValue: Int) { self.rawValue = rawValue }

    static let first    = TestOptions(rawValue: 1 << 0)
    static let second  = TestOptions(rawValue: 1 << 1)
    static let third   = TestOptions(rawValue: 1 << 2)
    static let fourth   = TestOptions(rawValue: 1 << 3)
}

// CHECK:      sil @{{.*}}returnTestOptions{{.*}}
// CHECK-NEXT: bb0:
// CHECK-NEXT:   integer_literal {{.*}}, 15
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   struct $TestOptions
// CHECK-NEXT:   return
public func returnTestOptions() -> TestOptions {
    return [.first, .second, .third, .fourth]
}

// CHECK:        alloc_global @{{.*}}globalTestOptions{{.*}}
// CHECK-NEXT:   global_addr
// CHECK-NEXT:   integer_literal {{.*}}, 15
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   debug_value
// CHECK-NEXT:   struct $TestOptions
// CHECK-NEXT:   store
// CHECK-NEXT:   tuple
// CHECK-NEXT:   return
let globalTestOptions: TestOptions = [.first, .second, .third, .fourth]


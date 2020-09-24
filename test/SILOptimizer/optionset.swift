// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -Osize -sil-verify-all -module-name=test -emit-sil | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

public struct TestOptions: OptionSet {
    public let rawValue: Int
    public init(rawValue: Int) { self.rawValue = rawValue }

    static let first    = TestOptions(rawValue: 1 << 0)
    static let second  = TestOptions(rawValue: 1 << 1)
    static let third   = TestOptions(rawValue: 1 << 2)
    static let fourth   = TestOptions(rawValue: 1 << 3)
}

// CHECK-LABEL: sil_global hidden [let] @$s4test17globalTestOptionsAA0cD0Vvp : $TestOptions = {
// CHECK:   [[CONST:%.*]] = integer_literal $Builtin.Int{{32|64}}, 15
// CHECK:   [[INT:%.*]] = struct $Int (%0 : $Builtin.Int{{32|64}})
// CHECK:   %initval = struct $TestOptions ([[INT]] : $Int)
let globalTestOptions: TestOptions = [.first, .second, .third, .fourth]

// CHECK-LABEL: sil @{{.*}}returnTestOptions{{.*}}
// CHECK-NEXT: bb0:
// CHECK-NEXT:   builtin
// CHECK-NEXT:   integer_literal {{.*}}, 15
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   struct $TestOptions
// CHECK-NEXT:   return
public func returnTestOptions() -> TestOptions {
    return [.first, .second, .third, .fourth]
}

// CHECK-LABEL: sil @{{.*}}returnEmptyTestOptions{{.*}}
// CHECK-NEXT: bb0:
// CHECK-NEXT:   integer_literal {{.*}}, 0
// CHECK-NEXT:   builtin "onFastPath"() : $()
// CHECK-NEXT:   struct $Int
// CHECK-NEXT:   struct $TestOptions
// CHECK-NEXT:   return
public func returnEmptyTestOptions() -> TestOptions {
    return []
}

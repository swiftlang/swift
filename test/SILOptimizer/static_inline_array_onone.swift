// RUN: %target-swift-frontend -parse-as-library -Onone -disable-availability-checking -module-name=test -emit-sil %s | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -parse-as-library -Onone -disable-availability-checking -module-name=test -emit-ir %s | %FileCheck %s --check-prefix=IR

// Check that a global InlineArray with a constant literal initializer is statically
// initialized and doesn't need a lazy `swift_once` initialization at -Onone.
// https://github.com/swiftlang/swift/issues/94263

// SIL-LABEL: sil_global hidden @$s4test3arrs11InlineArrayVy$3_s6UInt64VGvp : $InlineArray<4, UInt64> = {
// SIL:         %initval = struct $InlineArray<4, UInt64>
var arr: InlineArray<4, UInt64> = [1, 2, 3, 4]

// SIL-LABEL: sil_global hidden [section "__DATA,__mysec"] @$s4test6zeroess11InlineArrayVy$3_s6UInt64VGvp : $InlineArray<4, UInt64> = {
// SIL:         %initval = struct $InlineArray<4, UInt64>
@section("__DATA,__mysec")
var zeroes: InlineArray<4, UInt64> = [0, 0, 0, 0]

// SIL-LABEL: sil_global hidden @$s4test6nesteds11InlineArrayVy$1_ADy$1_SiGGvp : $InlineArray<2, InlineArray<2, Int>> = {
// SIL:         %initval = struct $InlineArray<2, InlineArray<2, Int>>
var nested: InlineArray<2, InlineArray<2, Int>> = [[1, 2], [3, 4]]

// SIL-NOT: builtin "once"
// IR-NOT:  call {{.*}}@swift_once

public func use() -> UInt64 {
  return arr[1] + zeroes[2] + UInt64(nested[1][0])
}

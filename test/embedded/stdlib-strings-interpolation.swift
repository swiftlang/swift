// RUN: %target-run-simple-swift(-Osize -swift-version 5 -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let n = 42
    let m = -42
    let s = "str"
    let str = "Hello \(s) \(n) \(s) \(m)"
    print(str)

    print("hex: \(hex: 42)")
    print("ptr: \(UnsafeRawPointer(bitPattern: UInt(0xffff0000))!)")
  }
}

extension DefaultStringInterpolation {
  mutating func appendInterpolation(hex value: Int) {
    appendInterpolation("0x" + String(value, radix: 16))
  }

  mutating func appendInterpolation(_ value: UnsafeRawPointer) {
    appendInterpolation("0x" + String(UInt(bitPattern: value), radix: 16))
  }
}

// CHECK: Hello str 42 str -42
// CHECK: hex: 0x2a
// CHECK: ptr: 0xffff0000

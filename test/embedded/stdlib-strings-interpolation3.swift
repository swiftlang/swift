// RUN: %target-swift-frontend -target %target-triple -module-name main -parse-as-library -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -target %target-triple -module-name main -parse-as-library -emit-ir %s -enable-experimental-feature Embedded -O
// RUN: %target-swift-frontend -target %target-triple -module-name main -parse-as-library -emit-ir %s -enable-experimental-feature Embedded -Osize

// REQUIRES: swift_in_compiler
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
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

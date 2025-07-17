// RUN: %target-swift-frontend -target armv7-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-frontend -target arm64-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

// https://github.com/apple/swift/issues/73249
// UNSUPPORTED: OS=windows-msvc

protocol MyOptionSet: Equatable {
  init(rawValue: Int)
  init()
}

extension MyOptionSet {
  var isEmpty: Bool {
    return self == Self()
  }
  init() {
    self.init(rawValue: 0)
  }
}

struct ShippingOptions: MyOptionSet {
  let rawValue: Int
}

var s = ShippingOptions(rawValue: 42)
print(s.isEmpty)

// CHECK: define {{.*}}i32 @main(i32 %0, ptr %1)

// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -wmo -runtime-compatibility-version none -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

struct ShippingOptions: OptionSet {
  let rawValue: Int

  static let nextDay    = ShippingOptions(rawValue: 1 << 0)
  static let secondDay  = ShippingOptions(rawValue: 1 << 1)
  static let priority   = ShippingOptions(rawValue: 1 << 2)
  static let standard   = ShippingOptions(rawValue: 1 << 3)

  static let express: ShippingOptions = [.nextDay, .secondDay]
  static let all: ShippingOptions = [.express, .priority, .standard]
}

let s = ShippingOptions.all
print("ShippingOptions.all = ")
print(s.rawValue)

// CHECK: ShippingOptions.all =
// CHECK-NEXT: 15

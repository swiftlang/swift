// RUN: %empty-directory(%t)

// RUN: %target-swiftc_driver -emit-sib -module-name mysibmodule %s -o - | %target-swiftc_driver -emit-module -module-name mysibmodule -o %t/mysibmodule.swiftmodule -

/// Since -validate-tbd-against-ir=default, TBD validation is not done since the module has a SIB file
// RUN: %target-swift-frontend -emit-sil %t/mysibmodule.swiftmodule | %FileCheck %s

// RUN: %target-swiftc_driver -emit-module -module-name mynonsibmodule -o %t/mynonsibmodule.swiftmodule %s

/// Since -validate-tbd-against-ir=default, TBD validation is done or not depending on the build configuration
// RUN: %target-swift-frontend -emit-sil %t/mynonsibmodule.swiftmodule | %FileCheck %s

public class MyClass {
  var x : Int

  public init(input : Int) {
    x = 2 * input
  }

  public func do_something(input : Int) -> Int {
    return x * input
  }
}

// CHECK: class MyClass

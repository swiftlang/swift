// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name exclusivity
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name exclusivity
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: @exclusivity(checked) public var checkedGlobalVar: Swift.Int
@exclusivity(checked)
public var checkedGlobalVar = 1

// CHECK: @exclusivity(unchecked) public var uncheckedGlobalVar: Swift.Int
@exclusivity(unchecked)
public var uncheckedGlobalVar = 1

// CHECK-LABEL: public struct Struct
public struct Struct {
  // CHECK: @exclusivity(unchecked) public static var uncheckedStaticVar: Swift.Int
  @exclusivity(unchecked)
  public static var uncheckedStaticVar: Int = 27

  // CHECK: @exclusivity(checked) public static var checkedStaticVar: Swift.Int
  @exclusivity(checked)
  public static var checkedStaticVar: Int = 27
}

// CHECK-LABEL: public class Class
public class Class {
  // CHECK: @exclusivity(unchecked) public var uncheckedInstanceVar: Swift.Int
  @exclusivity(unchecked)
  public var uncheckedInstanceVar: Int = 27

  // CHECK: @exclusivity(checked) public var checkedInstanceVar: Swift.Int
  @exclusivity(checked)
  public var checkedInstanceVar: Int = 27

  // CHECK:      @exclusivity(unchecked) public var uncheckedPrivateSetInstanceVar: Swift.Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  @exclusivity(unchecked)
  public private(set) var uncheckedPrivateSetInstanceVar: Int = 27

  // CHECK: @exclusivity(unchecked) public static var uncheckedStaticVar: Swift.Int
  @exclusivity(unchecked)
  public static var uncheckedStaticVar: Int = 27
}

// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -parse-as-library -O -emit-sil | %FileCheck %s
//
// Test speculative devirtualization.

// Test MaxNumSpeculativeTargets.
// rdar:23228386
public class Base {
  public init() {}
  public func foo() {}
}
class Sub1 : Base {
  override func foo() {}
}
class Sub2 : Base {
  override func foo() {}
}
class Sub3 : Base {
  override func foo() {}
}
class Sub4 : Base {
  override func foo() {}
}
class Sub5 : Base {
  override func foo() {}
}
class Sub6 : Base {
  override func foo() {}
}
class Sub7 : Base {
  override func foo() {}
}
// CHECK: @_T016devirt_speculate28testMaxNumSpeculativeTargetsyAA4BaseCF
// CHECK: checked_cast_br [exact] %0 : $Base to $Base
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub1
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub2
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub3
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub4
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub5
// CHECK: checked_cast_br [exact] %0 : $Base to $Sub6
// CHECK-NOT: checked_cast_br
// CHECK: %[[CM:[0-9]+]] = class_method %0 : $Base, #Base.foo!1 : (Base) -> () -> (), $@convention(method) (@guaranteed Base) -> ()
// CHECK: apply %[[CM]](%0) : $@convention(method) (@guaranteed Base) -> ()
public func testMaxNumSpeculativeTargets(_ b: Base) {
  b.foo()
}

// RUN: %target-swift-frontend %/s -parse-as-library -O -emit-sil -save-optimization-record-path %t.opt.yaml | %FileCheck %s
// RUN: %FileCheck -check-prefix=YAML -input-file=%t.opt.yaml %s
// RUN: %target-swift-frontend %/s -parse-as-library -Osize -emit-sil | %FileCheck %s --check-prefix=OSIZE
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
// CHECK: @$s16devirt_speculate28testMaxNumSpeculativeTargetsyyAA4BaseCF
// CHECK: checked_cast_br [exact] %0 : $Base to Base
// CHECK: checked_cast_br [exact] %0 : $Base to Sub1
// CHECK: checked_cast_br [exact] %0 : $Base to Sub2
// CHECK: checked_cast_br [exact] %0 : $Base to Sub3
// CHECK: checked_cast_br [exact] %0 : $Base to Sub4
// CHECK: checked_cast_br [exact] %0 : $Base to Sub5
// CHECK: checked_cast_br [exact] %0 : $Base to Sub6
// CHECK-NOT: checked_cast_br
// CHECK: %[[CM:[0-9]+]] = class_method %0 : $Base, #Base.foo!1 : (Base) -> () -> (), $@convention(method) (@guaranteed Base) -> ()
// CHECK: apply %[[CM]](%0) : $@convention(method) (@guaranteed Base) -> ()

// YAML:      Pass:            sil-speculative-devirtualizer
// YAML-NEXT: Name:            sil.PartialSpecDevirt
// YAML-NEXT: DebugLoc:
// YAML-NEXT:   File:            {{.*}}/devirt_speculate.swift
// YAML-NEXT:   Line:            66
// YAML-NEXT:   Column:          5
// YAML-NEXT: Function:        'testMaxNumSpeculativeTargets(_:)'
// YAML-NEXT: Args:
// YAML-NEXT:   - String:          'Partially devirtualized call with run-time checks for '
// YAML-NEXT:   - NumSubTypesChecked: '6'
// YAML-NEXT:   - String:          ' subclasses of '
// YAML-NEXT:   - ClassType:       Base
// YAML-NEXT:   - String:          ', number of subclasses not devirtualized: '
// YAML-NEXT:   - NotHandledSubsNum: '1'
// YAML-NEXT: ...

// OSIZE: @$s16devirt_speculate28testMaxNumSpeculativeTargetsyyAA4BaseCF
// OSIZE-NOT: checked_cast_br [exact] %0 : $Base to Base
// OSIZE-NOT: checked_cast_br [exact] %0 : $Base to Sub
public func testMaxNumSpeculativeTargets(_ b: Base) {
  b.foo()
}

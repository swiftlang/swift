// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -primary-file %s %S/Inputs/sil-merge-partial-modules-other.swift -module-name test -enable-resilience -o %t/partial.a.swiftmodule
// RUN: %target-swift-frontend -emit-module %s -primary-file %S/Inputs/sil-merge-partial-modules-other.swift -module-name test -enable-resilience -o %t/partial.b.swiftmodule

// RUN: %target-swift-frontend -emit-module %t/partial.a.swiftmodule %t/partial.b.swiftmodule -module-name test -enable-resilience -sil-merge-partial-modules -disable-diagnostic-passes -disable-sil-perf-optzns -o %t/test.swiftmodule

// RUN: %target-sil-opt %t/test.swiftmodule -disable-sil-linking > %t/dump.sil
// RUN: %FileCheck %s < %t/dump.sil
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/dump.sil

public func publicFunction() {
  internalFunction()
}

@inlinable
public func inlinableFunction() {
  let fn = { versionedFunction() }

  fn()
}

@_fixed_layout
public struct Rectangle : Shape {
  @inlinable
  public func draw() {
    publicFunction()
  }

  public var area: Float { return 10.0 }
}

public struct Circle : Shape {
  public func draw() {}

  public var area: Float { return 22.0 / 7 }
}

public class CircleManager : ShapeManager {
  public override func manage() {}
}

// FIXME: Why is the definition order totally random?

// CHECK-LABEL: sil [canonical] @$S4test17versionedFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil [canonical] @$S4test9RectangleV4areaSfvg : $@convention(method) (Rectangle) -> Float

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [canonical] @$S4test9RectangleVAA5ShapeA2aDP4drawyyFTW : $@convention(witness_method: Shape) (@in_guaranteed Rectangle) -> () {
// CHECK: function_ref @$S4test14publicFunctionyyF
// CHECK: }

// CHECK-LABEL: sil [canonical] @$S4test14publicFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [canonical] @$S4test9RectangleVAA5ShapeA2aDP4areaSfvgTW : $@convention(witness_method: Shape) (@in_guaranteed Rectangle) -> Float {
// CHECK: function_ref @$S4test9RectangleV4areaSfvg
// CHECK: }

// CHECK-LABEL: sil shared [serialized] [canonical] @$S4test17inlinableFunctionyyFyycfU_ : $@convention(thin) () -> () {
// CHECK: function_ref @$S4test17versionedFunctionyyF
// CHECK: }

// CHECK-LABEL: sil [serialized] [canonical] @$S4test17inlinableFunctionyyF : $@convention(thin) () -> () {
// CHECK: function_ref @$S4test17inlinableFunctionyyFyycfU_
// CHECK: }

// CHECK-LABEL: sil_witness_table [serialized] Rectangle: Shape module test {
// CHECK-LABEL:   method #Shape.draw!1: <Self where Self : Shape> (Self) -> () -> () : @$S4test9RectangleVAA5ShapeA2aDP4drawyyFTW
// CHECK-LABEL:   method #Shape.area!getter.1: <Self where Self : Shape> (Self) -> () -> Float : @$S4test9RectangleVAA5ShapeA2aDP4areaSfvgTW
// CHECK-LABEL: }

// NEGATIVE-NOT: sil {{.*}}internalFunction

// NEGATIVE-NOT: sil_witness_table {{.*}}Circle: Shape

// NEGATIVE-NOT: sil_vtable

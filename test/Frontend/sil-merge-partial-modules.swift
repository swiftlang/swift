// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -primary-file %s %S/Inputs/sil-merge-partial-modules-other.swift -module-name test -enable-library-evolution -o %t/partial.a.swiftmodule
// RUN: %target-swift-frontend -emit-module %s -primary-file %S/Inputs/sil-merge-partial-modules-other.swift -module-name test -enable-library-evolution -o %t/partial.b.swiftmodule

// RUN: %target-swift-frontend -merge-modules -emit-module %t/partial.a.swiftmodule %t/partial.b.swiftmodule -module-name test -enable-library-evolution -o %t/test.swiftmodule

// RUN: %target-sil-opt %t/test.swiftmodule -emit-sorted-sil > %t/dump.sil
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

@frozen
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

// CHECK-LABEL: sil [canonical] @$s4test14publicFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil [serialized] [canonical] [ossa] @$s4test17inlinableFunctionyyF : $@convention(thin) () -> () {
// CHECK: function_ref @$s4test17inlinableFunctionyyFyycfU_
// CHECK: }

// CHECK-LABEL: sil shared [serialized] [canonical] [ossa] @$s4test17inlinableFunctionyyFyycfU_ : $@convention(thin) () -> () {
// CHECK: function_ref @$s4test17versionedFunctionyyF
// CHECK: }

// CHECK-LABEL: sil [canonical] @$s4test17versionedFunctionyyF : $@convention(thin) () -> ()

// CHECK-LABEL: sil [canonical] @$s4test9RectangleV4areaSfvg : $@convention(method) (Rectangle) -> Float

// CHECK-LABEL: sil [serialized] [canonical] [ossa] @$s4test9RectangleV4drawyyF : $@convention(method) (Rectangle) -> () {
// CHECK: function_ref @$s4test14publicFunctionyyF
// CHECK: }

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s4test9RectangleVAA5ShapeA2aDP4areaSfvgTW :
// CHECK: function_ref @$s4test9RectangleV4areaSfvg
// CHECK: }

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s4test9RectangleVAA5ShapeA2aDP4drawyyFTW :
// CHECK: function_ref @$s4test9RectangleV4drawyyF
// CHECK: }

// CHECK-LABEL: sil_witness_table [serialized] Rectangle: Shape module test {
// CHECK-LABEL:   method #Shape.draw: <Self where Self : Shape> (Self) -> () -> () : @$s4test9RectangleVAA5ShapeA2aDP4drawyyFTW
// CHECK-LABEL:   method #Shape.area!getter: <Self where Self : Shape> (Self) -> () -> Float : @$s4test9RectangleVAA5ShapeA2aDP4areaSfvgTW
// CHECK-LABEL: }

// NEGATIVE-NOT: sil {{.*}}internalFunction

// NEGATIVE-NOT: sil_witness_table {{.*}}Circle: Shape

// NEGATIVE-NOT: sil_vtable

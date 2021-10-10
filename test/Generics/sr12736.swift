// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr12736.(file).ColorModel@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Float32Components.Model, Self.Float32Components : ColorComponents>
public protocol ColorModel {
  associatedtype Float32Components: ColorComponents where Float32Components.Model == Self
}

public protocol ColorComponents {
  associatedtype Model: ColorModel
}

public protocol ColorPixel {
  associatedtype Model: ColorModel
}

// CHECK: sr12736.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.A : ColorPixel, Self.B : ColorPixel, Self.A.Model == Self.B.Model>
public protocol P {
  associatedtype A: ColorPixel
  associatedtype B: ColorPixel where B.Model == A.Model
}

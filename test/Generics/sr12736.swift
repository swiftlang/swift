// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: sr12736.(file).ColorModel@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[ColorModel]Float32Components.[ColorComponents]Model, Self.[ColorModel]Float32Components : ColorComponents>
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
// CHECK-NEXT: Requirement signature: <Self where Self.[P]A : ColorPixel, Self.[P]B : ColorPixel, Self.[P]A.[ColorPixel]Model == Self.[P]B.[ColorPixel]Model>
public protocol P {
  associatedtype A: ColorPixel
  associatedtype B: ColorPixel where B.Model == A.Model
}

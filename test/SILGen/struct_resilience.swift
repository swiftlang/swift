// RUN: %target-swift-frontend -I %S/Inputs -enable-source-import -emit-silgen -enable-resilience %s | FileCheck %s

import resilient_struct

// Resilient structs are always address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience26functionWithResilientTypesFTV16resilient_struct4Size1fFS1_S1__S1_ : $@convention(thin) (@out Size, @in Size, @owned @callee_owned (@out Size, @in Size) -> ()) -> ()
// CHECK:       bb0(%0 : $*Size, %1 : $*Size, %2 : $@callee_owned (@out Size, @in Size) -> ()):
// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]]#1 : $*Size
// CHECK:         apply %2(%0, [[SIZE_BOX]]#1)
func functionWithResilientTypes(s: Size, f: Size -> Size) -> Size {
  return f(s)
}

// Fixed-layout structs may be trivial or loadable

// CHECK-LABEL: sil hidden @_TF17struct_resilience28functionWithFixedLayoutTypesFTV16resilient_struct5Point1fFS1_S1__S1_ : $@convention(thin) (Point, @owned @callee_owned (Point) -> Point) -> Point
// CHECK:       bb0(%0 : $Point, %1 : $@callee_owned (Point) -> Point):
// CHECK:         [[NEW_POINT:%.*]] = apply %1(%0)
// CHECK:         return [[NEW_POINT]]
func functionWithFixedLayoutTypes(p: Point, f: Point -> Point) -> Point {
  return f(p)
}

// Fixed-layout struct with resilient stored properties is still address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience39functionWithFixedLayoutOfResilientTypesFTV16resilient_struct9Rectangle1fFS1_S1__S1_ : $@convention(thin) (@out Rectangle, @in Rectangle, @owned @callee_owned (@out Rectangle, @in Rectangle) -> ()) -> ()
// CHECK:        bb0(%0 : $*Rectangle, %1 : $*Rectangle, %2 : $@callee_owned (@out Rectangle, @in Rectangle) -> ()):
func functionWithFixedLayoutOfResilientTypes(r: Rectangle, f: Rectangle -> Rectangle) -> Rectangle {
  return f(r)
}

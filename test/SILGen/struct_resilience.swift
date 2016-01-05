// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-silgen -enable-resilience %s | FileCheck %s

import resilient_struct

// Resilient structs are always address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience26functionWithResilientTypesFTV16resilient_struct4Size1fFS1_S1__S1_ : $@convention(thin) (@out Size, @in Size, @owned @callee_owned (@out Size, @in Size) -> ()) -> ()
// CHECK:       bb0(%0 : $*Size, %1 : $*Size, %2 : $@callee_owned (@out Size, @in Size) -> ()):
func functionWithResilientTypes(s: Size, f: Size -> Size) -> Size {

  // Stored properties of resilient structs from outside our resilience
  // domain are accessed through accessors

// CHECK:         copy_addr %1 to [initialization] [[OTHER_SIZE_BOX:%.*]]#1 : $*Size
  var s2 = s

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_TFV16resilient_struct4Sizeg1wSi : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
// CHECK:         [[FN:%.*]] = function_ref @_TFV16resilient_struct4Sizes1wSi : $@convention(method) (Int, @inout Size) -> ()
// CHECK:         apply [[FN]]([[RESULT]], [[OTHER_SIZE_BOX]]#1)
  s2.w = s.w

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_TFV16resilient_struct4Sizeg1hSi : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
  _ = s.h

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         apply %2(%0, [[SIZE_BOX]])
// CHECK:         return
  return f(s)
}

// Fixed-layout structs may be trivial or loadable

// CHECK-LABEL: sil hidden @_TF17struct_resilience28functionWithFixedLayoutTypesFTV16resilient_struct5Point1fFS1_S1__S1_ : $@convention(thin) (Point, @owned @callee_owned (Point) -> Point) -> Point
// CHECK:       bb0(%0 : $Point, %1 : $@callee_owned (Point) -> Point):
func functionWithFixedLayoutTypes(p: Point, f: Point -> Point) -> Point {

  // Stored properties of fixed layout structs are accessed directly
  var p2 = p

// CHECK:         [[RESULT:%.*]] = struct_extract %0 : $Point, #Point.x
// CHECK:         [[DEST:%.*]] = struct_element_addr [[POINT_BOX:%.*]]#1 : $*Point, #Point.x
// CHECK:         assign [[RESULT]] to [[DEST]] : $*Int
  p2.x = p.x

// CHECK:         [[RESULT:%.*]] = struct_extract %0 : $Point, #Point.y
  _ = p.y

// CHECK:         [[NEW_POINT:%.*]] = apply %1(%0)
// CHECK:         return [[NEW_POINT]]
  return f(p)
}

// Fixed-layout struct with resilient stored properties is still address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience39functionWithFixedLayoutOfResilientTypesFTV16resilient_struct9Rectangle1fFS1_S1__S1_ : $@convention(thin) (@out Rectangle, @in Rectangle, @owned @callee_owned (@out Rectangle, @in Rectangle) -> ()) -> ()
// CHECK:        bb0(%0 : $*Rectangle, %1 : $*Rectangle, %2 : $@callee_owned (@out Rectangle, @in Rectangle) -> ()):
func functionWithFixedLayoutOfResilientTypes(r: Rectangle, f: Rectangle -> Rectangle) -> Rectangle {
  return f(r)
}

// Make sure we generate getters and setters for stored properties of
// resilient structs

public struct MySize {

// CHECK-LABEL: sil @_TFV17struct_resilience6MySizeg1wSi : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizes1wSi : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizem1wSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout MySize, @thick MySize.Type) -> ()>)
  public var w: Int

// CHECK-LABEL: sil @_TFV17struct_resilience6MySizeg1hSi : $@convention(method) (@in_guaranteed MySize) -> Int
  public let h: Int
}

// CHECK-LABEL: sil @_TF17struct_resilience28functionWithMyResilientTypesFTVS_6MySize1fFS0_S0__S0_ : $@convention(thin) (@out MySize, @in MySize, @owned @callee_owned (@out MySize, @in MySize) -> ()) -> ()
public func functionWithMyResilientTypes(s: MySize, f: MySize -> MySize) -> MySize {

  // Stored properties of resilient structs from inside our resilience
  // domain are accessed directly

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]]#1 : $*MySize
  var s2 = s

// CHECK:         [[SRC_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.w
// CHECK:         [[SRC:%.*]] = load [[SRC_ADDR]] : $*Int
// CHECK:         [[DEST_ADDR:%.*]] = struct_element_addr [[SIZE_BOX]]#1 : $*MySize, #MySize.w
// CHECK:         assign [[SRC]] to [[DEST_ADDR]] : $*Int
  s2.w = s.w

// CHECK:         [[RESULT_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.h
// CHECK:         [[RESULT:%.*]] = load [[RESULT_ADDR]] : $*Int
  _ = s.h

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*MySize
// CHECK:         apply %2(%0, [[SIZE_BOX]])
// CHECK:         return
  return f(s)
}

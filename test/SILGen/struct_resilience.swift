// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-silgen -enable-resilience %s | %FileCheck %s

import resilient_struct

// Resilient structs are always address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience26functionWithResilientTypesFTV16resilient_struct4Size1fFS1_S1__S1_ : $@convention(thin) (@in Size, @owned @callee_owned (@in Size) -> @out Size) -> @out Size
// CHECK:       bb0(%0 : $*Size, %1 : $*Size, %2 : $@callee_owned (@in Size) -> @out Size):
func functionWithResilientTypes(_ s: Size, f: (Size) -> Size) -> Size {

  // Stored properties of resilient structs from outside our resilience
  // domain are accessed through accessors

// CHECK:         copy_addr %1 to [initialization] [[OTHER_SIZE_BOX:%[0-9]*]] : $*Size
  var s2 = s

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_TFV16resilient_struct4Sizeg1wSi : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
// CHECK:         [[FN:%.*]] = function_ref @_TFV16resilient_struct4Sizes1wSi : $@convention(method) (Int, @inout Size) -> ()
// CHECK:         apply [[FN]]([[RESULT]], [[OTHER_SIZE_BOX]])
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

// Use materializeForSet for inout access of properties in resilient structs
// from a different resilience domain

func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @_TF17struct_resilience18resilientInOutTestFRV16resilient_struct4SizeT_ : $@convention(thin) (@inout Size) -> ()

func resilientInOutTest(_ s: inout Size) {

// CHECK:         function_ref @_TF17struct_resilience9inoutFuncFRSiT_
// CHECK:         function_ref @_TFV16resilient_struct4Sizem1wSi

  inoutFunc(&s.w)

// CHECK: return
}

// Fixed-layout structs may be trivial or loadable

// CHECK-LABEL: sil hidden @_TF17struct_resilience28functionWithFixedLayoutTypesFTV16resilient_struct5Point1fFS1_S1__S1_ : $@convention(thin) (Point, @owned @callee_owned (Point) -> Point) -> Point
// CHECK:       bb0(%0 : $Point, %1 : $@callee_owned (Point) -> Point):
func functionWithFixedLayoutTypes(_ p: Point, f: (Point) -> Point) -> Point {

  // Stored properties of fixed layout structs are accessed directly
  var p2 = p

// CHECK:         [[RESULT:%.*]] = struct_extract %0 : $Point, #Point.x
// CHECK:         [[DEST:%.*]] = struct_element_addr [[POINT_BOX:%[0-9]*]] : $*Point, #Point.x
// CHECK:         assign [[RESULT]] to [[DEST]] : $*Int
  p2.x = p.x

// CHECK:         [[RESULT:%.*]] = struct_extract %0 : $Point, #Point.y
  _ = p.y

// CHECK:         [[NEW_POINT:%.*]] = apply %1(%0)
// CHECK:         return [[NEW_POINT]]
  return f(p)
}

// Fixed-layout struct with resilient stored properties is still address-only

// CHECK-LABEL: sil hidden @_TF17struct_resilience39functionWithFixedLayoutOfResilientTypesFTV16resilient_struct9Rectangle1fFS1_S1__S1_ : $@convention(thin) (@in Rectangle, @owned @callee_owned (@in Rectangle) -> @out Rectangle) -> @out Rectangle
// CHECK:        bb0(%0 : $*Rectangle, %1 : $*Rectangle, %2 : $@callee_owned (@in Rectangle) -> @out Rectangle):
func functionWithFixedLayoutOfResilientTypes(_ r: Rectangle, f: (Rectangle) -> Rectangle) -> Rectangle {
  return f(r)
}

// Make sure we generate getters and setters for stored properties of
// resilient structs

public struct MySize {

  // Static computed property

// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizeg10expirationSi : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizes10expirationSi : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizem10expirationSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var expiration: Int {
    get { return copyright + 70 }
    set { copyright = newValue - 70 }
  }

  // Instance computed property

// CHECK-LABEL: sil @_TFV17struct_resilience6MySizeg1dSi : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizes1dSi : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizem1dSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var d: Int {
    get { return 0 }
    set { }
  }

  // Instance stored property

// CHECK-LABEL: sil @_TFV17struct_resilience6MySizeg1wSi : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizes1wSi : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_TFV17struct_resilience6MySizem1wSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var w: Int

  // Read-only instance stored property

// CHECK-LABEL: sil @_TFV17struct_resilience6MySizeg1hSi : $@convention(method) (@in_guaranteed MySize) -> Int
  public let h: Int

  // Static stored property

// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizeg9copyrightSi : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizes9copyrightSi : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_TZFV17struct_resilience6MySizem9copyrightSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var copyright: Int = 0
}

// CHECK-LABEL: sil @_TF17struct_resilience28functionWithMyResilientTypesFTVS_6MySize1fFS0_S0__S0_ : $@convention(thin) (@in MySize, @owned @callee_owned (@in MySize) -> @out MySize) -> @out MySize
public func functionWithMyResilientTypes(_ s: MySize, f: (MySize) -> MySize) -> MySize {

  // Stored properties of resilient structs from inside our resilience
  // domain are accessed directly

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%[0-9]*]] : $*MySize
  var s2 = s

// CHECK:         [[SRC_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.w
// CHECK:         [[SRC:%.*]] = load [[SRC_ADDR]] : $*Int
// CHECK:         [[DEST_ADDR:%.*]] = struct_element_addr [[SIZE_BOX]] : $*MySize, #MySize.w
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

// CHECK-LABEL: sil [transparent] [fragile] @_TF17struct_resilience25publicTransparentFunctionFVS_6MySizeSi : $@convention(thin) (@in MySize) -> Int
@_transparent public func publicTransparentFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_TFV17struct_resilience6MySizeg1wSi
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[GETTER]]([[SELF]])
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [transparent] [fragile] @_TF17struct_resilience30publicTransparentLocalFunctionFVS_6MySizeFT_Si : $@convention(thin) (@in MySize) -> @owned @callee_owned () -> Int
@_transparent public func publicTransparentLocalFunction(_ s: MySize) -> () -> Int {

// CHECK-LABEL: sil shared [fragile] @_TFF17struct_resilience30publicTransparentLocalFunctionFVS_6MySizeFT_SiU_FT_Si : $@convention(thin) (@owned @box MySize) -> Int
// CHECK: function_ref @_TFV17struct_resilience6MySizeg1wSi : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK: return {{.*}} : $Int

  return { s.w }

}

// CHECK-LABEL: sil hidden [transparent] @_TF17struct_resilience27internalTransparentFunctionFVS_6MySizeSi : $@convention(thin) (@in MySize) -> Int
@_transparent func internalTransparentFunction(_ s: MySize) -> Int {

  // The body of an internal transparent function will not be inlined into
  // other resilience domains, so we can access storage directly

// CHECK:         [[W_ADDR:%.*]] = struct_element_addr %0 : $*MySize, #MySize.w
// CHECK-NEXT:    [[RESULT:%.*]] = load [[W_ADDR]] : $*Int
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [fragile] [always_inline] @_TF17struct_resilience26publicInlineAlwaysFunctionFVS_6MySizeSi : $@convention(thin) (@in MySize) -> Int
@inline(__always) public func publicInlineAlwaysFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_TFV17struct_resilience6MySizeg1wSi
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[GETTER]]([[SELF]])
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    return [[RESULT]]
  return s.w

}

// Make sure that @_versioned entities can be resilient

@_versioned struct VersionedResilientStruct {
  @_versioned let x: Int
  @_versioned let y: Int

  @_versioned init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

// CHECK-LABEL: sil [transparent] [fragile] @_TF17struct_resilience27useVersionedResilientStructFVS_24VersionedResilientStructS0_ : $@convention(thin) (@in VersionedResilientStruct) -> @out VersionedResilientStruct
@_versioned
@_transparent func useVersionedResilientStruct(_ s: VersionedResilientStruct)
    -> VersionedResilientStruct {
  // CHECK:       function_ref @_TFV17struct_resilience24VersionedResilientStructCfT1xSi1ySi_S0_
  // CHECK:       function_ref @_TFV17struct_resilience24VersionedResilientStructg1ySi
  // CHECK:       function_ref @_TFV17struct_resilience24VersionedResilientStructg1xSi

  return VersionedResilientStruct(x: s.y, y: s.x)
}

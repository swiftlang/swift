// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -I %t -emit-silgen -enable-resilience %s | %FileCheck %s

import resilient_struct

// Resilient structs are always address-only

// CHECK-LABEL: sil hidden @_T017struct_resilience26functionWithResilientTypes010resilient_A04SizeVAE_AeEc1ftF : $@convention(thin) (@in Size, @owned @callee_owned (@in Size) -> @out Size) -> @out Size
// CHECK:       bb0(%0 : $*Size, %1 : $*Size, %2 : $@callee_owned (@in Size) -> @out Size):
func functionWithResilientTypes(_ s: Size, f: (Size) -> Size) -> Size {

  // Stored properties of resilient structs from outside our resilience
  // domain are accessed through accessors

// CHECK:         copy_addr %1 to [initialization] [[OTHER_SIZE_BOX:%[0-9]*]] : $*Size
  var s2 = s

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_T016resilient_struct4SizeV1wSifg : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
// CHECK:         [[FN:%.*]] = function_ref @_T016resilient_struct4SizeV1wSifs : $@convention(method) (Int, @inout Size) -> ()
// CHECK:         apply [[FN]]([[RESULT]], [[OTHER_SIZE_BOX]])
  s2.w = s.w

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_T016resilient_struct4SizeV1hSifg : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
  _ = s.h

// CHECK:         [[COPIED_CLOSURE:%.*]] = copy_value %2
// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         apply [[COPIED_CLOSURE]](%0, [[SIZE_BOX]])
// CHECK:         return
  return f(s)
}

// Use materializeForSet for inout access of properties in resilient structs
// from a different resilience domain

func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @_T017struct_resilience18resilientInOutTesty0c1_A04SizeVzF : $@convention(thin) (@inout Size) -> ()

func resilientInOutTest(_ s: inout Size) {

// CHECK:         function_ref @_T017struct_resilience9inoutFuncySizF
// CHECK:         function_ref @_T016resilient_struct4SizeV1wSifm

  inoutFunc(&s.w)

// CHECK: return
}

// Fixed-layout structs may be trivial or loadable

// CHECK-LABEL: sil hidden @_T017struct_resilience28functionWithFixedLayoutTypes010resilient_A05PointVAE_AeEc1ftF : $@convention(thin) (Point, @owned @callee_owned (Point) -> Point) -> Point
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

// CHECK:         [[COPIED_CLOSURE:%.*]] = copy_value %1
// CHECK:         [[NEW_POINT:%.*]] = apply [[COPIED_CLOSURE]](%0)
// CHECK:         return [[NEW_POINT]]
  return f(p)
}

// Fixed-layout struct with resilient stored properties is still address-only

// CHECK-LABEL: sil hidden @_T017struct_resilience39functionWithFixedLayoutOfResilientTypes010resilient_A09RectangleVAE_AeEc1ftF : $@convention(thin) (@in Rectangle, @owned @callee_owned (@in Rectangle) -> @out Rectangle) -> @out Rectangle
// CHECK:        bb0(%0 : $*Rectangle, %1 : $*Rectangle, %2 : $@callee_owned (@in Rectangle) -> @out Rectangle):
func functionWithFixedLayoutOfResilientTypes(_ r: Rectangle, f: (Rectangle) -> Rectangle) -> Rectangle {
  return f(r)
}

// Make sure we generate getters and setters for stored properties of
// resilient structs

public struct MySize {

  // Static computed property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSifgZ : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSifsZ : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSifmZ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var expiration: Int {
    get { return copyright + 70 }
    set { copyright = newValue - 70 }
  }

  // Instance computed property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSifg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSifs : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var d: Int {
    get { return 0 }
    set { }
  }

  // Instance stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSifg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSifs : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var w: Int

  // Read-only instance stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1hSifg : $@convention(method) (@in_guaranteed MySize) -> Int
  public let h: Int

  // Static stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSifgZ : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSifsZ : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSifmZ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var copyright: Int = 0
}

// CHECK-LABEL: sil @_T017struct_resilience28functionWithMyResilientTypesAA0E4SizeVAD_AdDc1ftF : $@convention(thin) (@in MySize, @owned @callee_owned (@in MySize) -> @out MySize) -> @out MySize
public func functionWithMyResilientTypes(_ s: MySize, f: (MySize) -> MySize) -> MySize {

  // Stored properties of resilient structs from inside our resilience
  // domain are accessed directly

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%[0-9]*]] : $*MySize
  var s2 = s

// CHECK:         [[SRC_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.w
// CHECK:         [[SRC:%.*]] = load [trivial] [[SRC_ADDR]] : $*Int
// CHECK:         [[DEST_ADDR:%.*]] = struct_element_addr [[SIZE_BOX]] : $*MySize, #MySize.w
// CHECK:         assign [[SRC]] to [[DEST_ADDR]] : $*Int
  s2.w = s.w

// CHECK:         [[RESULT_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.h
// CHECK:         [[RESULT:%.*]] = load [trivial] [[RESULT_ADDR]] : $*Int
  _ = s.h

// CHECK:         [[BORROWED_CLOSURE:%.*]] = begin_borrow %2
// CHECK:         [[CLOSURE_COPY:%.*]] = copy_value [[BORROWED_CLOSURE]]
// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*MySize
// CHECK:         apply [[CLOSURE_COPY]](%0, [[SIZE_BOX]])
// CHECK:         return
  return f(s)
}

// CHECK-LABEL: sil [transparent] [fragile] @_T017struct_resilience25publicTransparentFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
@_transparent public func publicTransparentFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_T017struct_resilience6MySizeV1wSifg
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[GETTER]]([[SELF]])
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [transparent] [fragile] @_T017struct_resilience30publicTransparentLocalFunctionSiycAA6MySizeVF : $@convention(thin) (@in MySize) -> @owned @callee_owned () -> Int
@_transparent public func publicTransparentLocalFunction(_ s: MySize) -> () -> Int {

// CHECK-LABEL: sil shared [fragile] @_T017struct_resilience30publicTransparentLocalFunctionSiycAA6MySizeVFSiycfU_ : $@convention(thin) (@owned { var MySize }) -> Int
// CHECK: function_ref @_T017struct_resilience6MySizeV1wSifg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK: return {{.*}} : $Int

  return { s.w }

}

// CHECK-LABEL: sil hidden [transparent] @_T017struct_resilience27internalTransparentFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
// CHECK: bb0([[ARG:%.*]] : $*MySize):
@_transparent func internalTransparentFunction(_ s: MySize) -> Int {

  // The body of an internal transparent function will not be inlined into
  // other resilience domains, so we can access storage directly

// CHECK:         [[W_ADDR:%.*]] = struct_element_addr [[ARG]] : $*MySize, #MySize.w
// CHECK-NEXT:    [[RESULT:%.*]] = load [trivial] [[W_ADDR]] : $*Int
// CHECK-NEXT:    destroy_addr [[ARG]]
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [fragile] [always_inline] @_T017struct_resilience26publicInlineAlwaysFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
@inline(__always) public func publicInlineAlwaysFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_T017struct_resilience6MySizeV1wSifg
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

// CHECK-LABEL: sil [transparent] [fragile] @_T017struct_resilience27useVersionedResilientStructAA0deF0VADF : $@convention(thin) (@in VersionedResilientStruct) -> @out VersionedResilientStruct
@_versioned
@_transparent func useVersionedResilientStruct(_ s: VersionedResilientStruct)
    -> VersionedResilientStruct {
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructVACSi1x_Si1ytcfC
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructV1ySifg
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructV1xSifg

  return VersionedResilientStruct(x: s.y, y: s.x)
}

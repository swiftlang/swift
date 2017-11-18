// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -enable-sil-ownership -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -enable-sil-ownership -emit-silgen -enable-resilience %s | %FileCheck %s

import resilient_struct

// Resilient structs are always address-only

// CHECK-LABEL: sil hidden @_T017struct_resilience26functionWithResilientTypes010resilient_A04SizeVAE_A2Ec1ftF : $@convention(thin) (@in Size, @owned @noescape @callee_guaranteed (@in Size) -> @out Size) -> @out Size
// CHECK:       bb0(%0 : @trivial $*Size, %1 : @trivial $*Size, %2 : @owned $@noescape @callee_guaranteed (@in Size) -> @out Size):
func functionWithResilientTypes(_ s: Size, f: (Size) -> Size) -> Size {

  // Stored properties of resilient structs from outside our resilience
  // domain are accessed through accessors

// CHECK:         copy_addr %1 to [initialization] [[OTHER_SIZE_BOX:%[0-9]*]] : $*Size
  var s2 = s

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[GETTER:%.*]] = function_ref @_T016resilient_struct4SizeV1wSivg : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[GETTER]]([[SIZE_BOX]])
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] [[OTHER_SIZE_BOX]] : $*Size
// CHECK:         [[SETTER:%.*]] = function_ref @_T016resilient_struct4SizeV1wSivs : $@convention(method) (Int, @inout Size) -> ()
// CHECK:         apply [[SETTER]]([[RESULT]], [[WRITE]])
  s2.w = s.w

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[FN:%.*]] = function_ref @_T016resilient_struct4SizeV1hSivg : $@convention(method) (@in_guaranteed Size) -> Int
// CHECK:         [[RESULT:%.*]] = apply [[FN]]([[SIZE_BOX]])
  _ = s.h

// CHECK:         [[COPIED_CLOSURE:%.*]] = copy_value %2
// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*Size
// CHECK:         [[BORROW:%.*]] = begin_borrow [[COPIED_CLOSURE]]
// CHECK:         apply [[BORROW]](%0, [[SIZE_BOX]])
// CHECK:         destroy_value [[COPIED_CLOSURE]]
// CHECK:         return
  return f(s)
}

// Use materializeForSet for inout access of properties in resilient structs
// from a different resilience domain

public func inoutFunc(_ x: inout Int) {}

// CHECK-LABEL: sil hidden @_T017struct_resilience18resilientInOutTesty0c1_A04SizeVzF : $@convention(thin) (@inout Size) -> ()

func resilientInOutTest(_ s: inout Size) {

// CHECK:         function_ref @_T016resilient_struct4SizeV1wSivm
// CHECK:         function_ref @_T017struct_resilience9inoutFuncySizF

  inoutFunc(&s.w)

// CHECK: return
}

// Fixed-layout structs may be trivial or loadable

// CHECK-LABEL: sil hidden @_T017struct_resilience28functionWithFixedLayoutTypes010resilient_A05PointVAE_A2Ec1ftF : $@convention(thin) (Point, @owned @noescape @callee_guaranteed (Point) -> Point) -> Point
// CHECK:       bb0(%0 : @trivial $Point, %1 : @owned $@noescape @callee_guaranteed (Point) -> Point):
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
// CHECK:         [[BORROW:%.*]] = begin_borrow [[COPIED_CLOSURE]]
// CHECK:         [[NEW_POINT:%.*]] = apply [[BORROW]](%0)
// CHECK:         destroy_value [[COPIED_CLOSURE]]
// CHECK:         return [[NEW_POINT]]
  return f(p)
}

// Fixed-layout struct with resilient stored properties is still address-only

// CHECK-LABEL: sil hidden @_T017struct_resilience39functionWithFixedLayoutOfResilientTypes010resilient_A09RectangleVAE_A2Ec1ftF : $@convention(thin) (@in Rectangle, @owned @noescape @callee_guaranteed (@in Rectangle) -> @out Rectangle) -> @out Rectangle
// CHECK:        bb0(%0 : @trivial $*Rectangle, %1 : @trivial $*Rectangle, %2 : @owned $@noescape @callee_guaranteed (@in Rectangle) -> @out Rectangle):
func functionWithFixedLayoutOfResilientTypes(_ r: Rectangle, f: (Rectangle) -> Rectangle) -> Rectangle {
  return f(r)
}

// Make sure we generate getters and setters for stored properties of
// resilient structs

public struct MySize {

  // Static computed property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSivgZ : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSivsZ : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV10expirationSivmZ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var expiration: Int {
    get { return copyright + 70 }
    set { copyright = newValue - 70 }
  }

  // Instance computed property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSivg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSivs : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1dSivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var d: Int {
    get { return 0 }
    set { }
  }

  // Instance stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSivg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSivs : $@convention(method) (Int, @inout MySize) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1wSivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout MySize) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public var w: Int

  // Read-only instance stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV1hSivg : $@convention(method) (@in_guaranteed MySize) -> Int
  public let h: Int

  // Static stored property

// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSivgZ : $@convention(method) (@thin MySize.Type) -> Int
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSivsZ : $@convention(method) (Int, @thin MySize.Type) -> ()
// CHECK-LABEL: sil @_T017struct_resilience6MySizeV9copyrightSivmZ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @thin MySize.Type) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
  public static var copyright: Int = 0
}

// CHECK-LABEL: sil @_T017struct_resilience28functionWithMyResilientTypesAA0E4SizeVAD_A2Dc1ftF : $@convention(thin) (@in MySize, @owned @noescape @callee_guaranteed (@in MySize) -> @out MySize) -> @out MySize
public func functionWithMyResilientTypes(_ s: MySize, f: (MySize) -> MySize) -> MySize {

  // Stored properties of resilient structs from inside our resilience
  // domain are accessed directly

// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%[0-9]*]] : $*MySize
  var s2 = s

// CHECK:         [[SRC_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.w
// CHECK:         [[SRC:%.*]] = load [trivial] [[SRC_ADDR]] : $*Int
// CHECK:         [[WRITE:%.*]] = begin_access [modify] [unknown] [[SIZE_BOX]] : $*MySize
// CHECK:         [[DEST_ADDR:%.*]] = struct_element_addr [[WRITE]] : $*MySize, #MySize.w
// CHECK:         assign [[SRC]] to [[DEST_ADDR]] : $*Int
  s2.w = s.w

// CHECK:         [[RESULT_ADDR:%.*]] = struct_element_addr %1 : $*MySize, #MySize.h
// CHECK:         [[RESULT:%.*]] = load [trivial] [[RESULT_ADDR]] : $*Int
  _ = s.h

// CHECK:         [[BORROWED_CLOSURE:%.*]] = begin_borrow %2
// CHECK:         [[CLOSURE_COPY:%.*]] = copy_value [[BORROWED_CLOSURE]]
// CHECK:         copy_addr %1 to [initialization] [[SIZE_BOX:%.*]] : $*MySize
// CHECK:         [[BORROW:%.*]] = begin_borrow [[CLOSURE_COPY]]
// CHECK:         apply [[BORROW]](%0, [[SIZE_BOX]])
// CHECK:         destroy_value [[CLOSURE_COPY]]
// CHECK:         return
  return f(s)
}

// CHECK-LABEL: sil [transparent] [serialized] @_T017struct_resilience25publicTransparentFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
@_transparent public func publicTransparentFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_T017struct_resilience6MySizeV1wSivg
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[GETTER]]([[SELF]])
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [transparent] [serialized] @_T017struct_resilience30publicTransparentLocalFunctionSiycAA6MySizeVF : $@convention(thin) (@in MySize) -> @owned @callee_guaranteed () -> Int
@_transparent public func publicTransparentLocalFunction(_ s: MySize) -> () -> Int {

// CHECK-LABEL: sil shared [serialized] @_T017struct_resilience30publicTransparentLocalFunctionSiycAA6MySizeVFSiycfU_ : $@convention(thin) (@guaranteed { var MySize }) -> Int
// CHECK: function_ref @_T017struct_resilience6MySizeV1wSivg : $@convention(method) (@in_guaranteed MySize) -> Int
// CHECK: return {{.*}} : $Int

  return { s.w }

}

// CHECK-LABEL: sil hidden [transparent] @_T017struct_resilience27internalTransparentFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
// CHECK: bb0([[ARG:%.*]] : @trivial $*MySize):
@_transparent func internalTransparentFunction(_ s: MySize) -> Int {

  // The body of an internal transparent function will not be inlined into
  // other resilience domains, so we can access storage directly

// CHECK:         [[W_ADDR:%.*]] = struct_element_addr [[ARG]] : $*MySize, #MySize.w
// CHECK-NEXT:    [[RESULT:%.*]] = load [trivial] [[W_ADDR]] : $*Int
// CHECK-NEXT:    destroy_addr [[ARG]]
// CHECK-NEXT:    return [[RESULT]]
  return s.w
}

// CHECK-LABEL: sil [serialized] [always_inline] @_T017struct_resilience26publicInlineAlwaysFunctionSiAA6MySizeVF : $@convention(thin) (@in MySize) -> Int
@inline(__always) public func publicInlineAlwaysFunction(_ s: MySize) -> Int {

  // Since the body of a public transparent function might be inlined into
  // other resilience domains, we have to use accessors

// CHECK:         [[SELF:%.*]] = alloc_stack $MySize
// CHECK-NEXT:    copy_addr %0 to [initialization] [[SELF]]

// CHECK:         [[GETTER:%.*]] = function_ref @_T017struct_resilience6MySizeV1wSivg
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

  // Non-inlineable initializer, assigns to self -- treated as a root initializer

  // CHECK-LABEL: sil @_T017struct_resilience24VersionedResilientStructVA2C5other_tcfC : $@convention(method) (@in VersionedResilientStruct, @thin VersionedResilientStruct.Type) -> @out VersionedResilientStruct
  // CHECK:      [[SELF_BOX:%.*]] = alloc_box ${ var VersionedResilientStruct }
  // CHECK-NEXT: [[SELF_UNINIT:%.*]] = mark_uninitialized [rootself] [[SELF_BOX]]
  // CHECK:      return
  @_versioned init(other: VersionedResilientStruct) {
    self = other
  }

  // Inlineable initializer, assigns to self -- treated as a delegating initializer

  // CHECK-LABEL: sil [serialized] @_T017struct_resilience24VersionedResilientStructVA2C6other2_tcfC : $@convention(method) (@in VersionedResilientStruct, @thin VersionedResilientStruct.Type) -> @out VersionedResilientStruct
  // CHECK:      [[SELF_BOX:%.*]] = alloc_box ${ var VersionedResilientStruct }
  // CHECK-NEXT: [[SELF_UNINIT:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:      return
  @_versioned @_inlineable init(other2: VersionedResilientStruct) {
    self = other2
  }
}

// CHECK-LABEL: sil [transparent] [serialized] @_T017struct_resilience27useVersionedResilientStructAA0deF0VADF : $@convention(thin) (@in VersionedResilientStruct) -> @out VersionedResilientStruct
@_versioned
@_transparent func useVersionedResilientStruct(_ s: VersionedResilientStruct)
    -> VersionedResilientStruct {
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructV1ySivg
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructV1xSivg
  // CHECK:       function_ref @_T017struct_resilience24VersionedResilientStructVACSi1x_Si1ytcfC

  return VersionedResilientStruct(x: s.y, y: s.x)

  // CHECK:       return
}

// CHECK-LABEL: sil [serialized] @_T017struct_resilience19inlineableInoutTestyAA6MySizeVzF : $@convention(thin) (@inout MySize) -> ()
@_inlineable public func inlineableInoutTest(_ s: inout MySize) {
  // Inlineable functions can be inlined in other resiliene domains.
  //
  // Make sure we use materializeForSet for an inout access of a resilient struct
  // property inside an inlinable function.

  // CHECK:       function_ref @_T017struct_resilience6MySizeV1wSivm
  inoutFunc(&s.w)

  // CHECK:       return
}

// Initializers for resilient structs
extension Size {

  // CHECK-LABEL: sil hidden @_T016resilient_struct4SizeV0B11_resilienceEA2C5other_tcfC : $@convention(method) (@in Size, @thin Size.Type) -> @out Size
  // CHECK:      [[SELF_BOX:%.*]] = alloc_box ${ var Size }
  // CHECK-NEXT: [[SELF_UNINIT:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var Size }
  // CHECK:      return
  init(other: Size) {
    self = other
  }
}

// RUN: %target-swift-frontend -enable-library-evolution -disable-availability-checking -O -emit-sil %s
// RUN: %target-swift-frontend -enable-library-evolution -disable-availability-checking -Onone -emit-sil %s | %FileCheck %s

public protocol P {}

public struct Underlying : P {
}

public func returnOpaque() -> some P {
  return Underlying()
}

// CHECK-LABEL: sil [serialized] @$s19cast_folding_opaque23testCastOpaqueArchetypeAA10UnderlyingVyF
// CHECK:   [[O:%.*]] = alloc_stack $@_opaqueReturnTypeOf("$s19cast_folding_opaque12returnOpaqueQryF", 0)
// CHECK:   [[F:%.*]] = function_ref @$s19cast_folding_opaque12returnOpaqueQryF
// CHECK:   apply [[F]]([[O]])
// CHECK:   unconditional_checked_cast_addr @_opaqueReturnTypeOf{{.*}}in [[O]] : $*@_opaqueReturnTypeOf{{.*}}to Underlying in %0 : $*Underlying
@inlinable
public func testCastOpaqueArchetype() -> Underlying {
  return returnOpaque() as! Underlying
}

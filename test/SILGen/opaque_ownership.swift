// RUN: %target-swift-frontend -enable-sil-opaque-values -enable-sil-ownership -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen -parse-stdlib -parse-as-library -module-name Swift %s | %FileCheck %s

public typealias AnyObject = Builtin.AnyObject

precedencegroup CastingPrecedence {}

public protocol _ObjectiveCBridgeable {}

public protocol UnkeyedDecodingContainer {
  var isAtEnd: Builtin.Int1 { get }
}

public protocol Decoder {
  func unkeyedContainer() throws -> UnkeyedDecodingContainer
}

// Test open_existential_value ownership
// ---
// CHECK-LABEL: sil @_T0s11takeDecoderBi1_s0B0_p4from_tKF : $@convention(thin) (@in Decoder) -> (Builtin.Int1, @error Builtin.NativeObject) {
// CHECK: bb0(%0 : @owned $Decoder):
// CHECK:  [[BORROW1:%.*]] = begin_borrow %0 : $Decoder
// CHECK:  [[OPENED:%.*]] = open_existential_value [[BORROW1]] : $Decoder to $@opened("{{.*}}") Decoder
// CHECK:  [[WT:%.*]] = witness_method $@opened("{{.*}}") Decoder, #Decoder.unkeyedContainer!1 : <Self where Self : Decoder> (Self) -> () throws -> UnkeyedDecodingContainer, %4 : $@opened("{{.*}}") Decoder : $@convention(witness_method) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out UnkeyedDecodingContainer, @error Builtin.NativeObject)
// CHECK:  try_apply [[WT]]<@opened("{{.*}}") Decoder>([[OPENED]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out UnkeyedDecodingContainer, @error Builtin.NativeObject), normal bb2, error bb1
//
// CHECK:bb{{.*}}([[RET1:%.*]] : @owned $UnkeyedDecodingContainer):
// CHECK:  end_borrow [[BORROW1]] from %0 : $Decoder, $Decoder
// CHECK:  [[BORROW2:%.*]] = begin_borrow [[RET1]] : $UnkeyedDecodingContainer
// CHECK:  [[OPENED2:%.*]] = open_existential_value [[BORROW2]] : $UnkeyedDecodingContainer to $@opened("{{.*}}") UnkeyedDecodingContainer
// CHECK:  [[WT2:%.*]] = witness_method $@opened("{{.*}}") UnkeyedDecodingContainer, #UnkeyedDecodingContainer.isAtEnd!getter.1 : <Self where Self : UnkeyedDecodingContainer> (Self) -> () -> Builtin.Int1, [[OPENED2]] : $@opened("{{.*}}") UnkeyedDecodingContainer : $@convention(witness_method) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK:  [[RET2:%.*]] = apply [[WT2]]<@opened("{{.*}}") UnkeyedDecodingContainer>([[OPENED2]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK:  end_borrow [[BORROW2]] from [[RET1]] : $UnkeyedDecodingContainer, $UnkeyedDecodingContainer
// CHECK:  destroy_value [[RET1]] : $UnkeyedDecodingContainer
// CHECK:  destroy_value %0 : $Decoder
// CHECK:  return [[RET2]] : $Builtin.Int1
// CHECK-LABEL: } // end sil function '_T0s11takeDecoderBi1_s0B0_p4from_tKF'
public func takeDecoder(from decoder: Decoder) throws -> Builtin.Int1 {
  let container = try decoder.unkeyedContainer()
  return container.isAtEnd
}

// Test unsafe_bitwise_cast nontrivial ownership.
// ---
// CHECK-LABEL: sil @_T0s13unsafeBitCastq_x_q_m2totr0_lF : $@convention(thin) <T, U> (@in T, @thick U.Type) -> @out U {
// CHECK: bb0(%0 : @owned $T, %1 : @trivial $@thick U.Type):
// CHECK:   %4 = begin_borrow %0 : $T
// CHECK:   %5 = copy_value %4 : $T
// CHECK:   %6 = unchecked_bitwise_cast %5 : $T to $U
// CHECK:   %7 = copy_value %6 : $U
// CHECK:   destroy_value %5 : $T
// CHECK:   end_borrow %4 from %0 : $T, $T
// CHECK:   destroy_value %0 : $T
// CHECK:   return %7 : $U
// CHECK-LABEL: } // end sil function '_T0s13unsafeBitCastq_x_q_m2totr0_lF'
public func unsafeBitCast<T, U>(_ x: T, to type: U.Type) -> U {
  return Builtin.reinterpretCast(x)
}

#if os(OSX)
// Test open_existential_value used in a conversion context.
// ---
// 
public func _unsafeDowncastToAnyObject(fromAny any: Any) -> AnyObject {
  return any as AnyObject
}
#endif

public enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

public protocol IP {}

public protocol Seq {
  associatedtype Iterator : IP

  func makeIterator() -> Iterator
}

extension Seq where Self.Iterator == Self {
  public func makeIterator() -> Self {
    return self
  }
}

public struct EnumIter<Base : IP> : IP, Seq {
  internal var _base: Base

  public typealias Iterator = EnumIter<Base>
}

// Test passing a +1 RValue to @in_guaranteed.
// ---
// CHECK-LABEL: sil @_T0s7EnumSeqV12makeIterators0A4IterVy0D0QzGyF : $@convention(method) <Base where Base : Seq> (@in_guaranteed EnumSeq<Base>) -> @out EnumIter<Base.Iterator> {
// CHECK: bb0(%0 : @guaranteed $EnumSeq<Base>):
// CHECK: [[FN:%.*]] = function_ref @_T0s8EnumIterVAByxGx5_base_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// CHECK:  [[MT:%.*]] = metatype $@thin EnumIter<Base.Iterator>.Type
// CHECK:  [[WT:%.*]] = witness_method $Base, #Seq.makeIterator!1 : <Self where Self : Seq> (Self) -> () -> Self.Iterator : $@convention(witness_method) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  [[FIELD:%.*]] = struct_extract %0 : $EnumSeq<Base>, #EnumSeq._base
// CHECK:  [[COPY:%.*]] = copy_value [[FIELD]] : $Base
// CHECK:  [[BORROW:%.*]] = begin_borrow [[COPY]] : $Base
// CHECK:  [[ITER:%.*]] = apply [[WT]]<Base>([[BORROW]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  end_borrow [[BORROW]] from [[COPY]] : $Base, $Base
// CHECK:  destroy_value [[COPY]] : $Base
// CHECK:  [[RET:%.*]] = apply [[FN]]<Base.Iterator>([[ITER]], [[MT]]) : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
// CHECK:  return [[RET]] : $EnumIter<Base.Iterator>
// CHECK-LABEL: } // end sil function '_T0s7EnumSeqV12makeIterators0A4IterVy0D0QzGyF'
public struct EnumSeq<Base : Seq> : Seq {
  public typealias Iterator = EnumIter<Base.Iterator>

  internal var _base: Base

  public func makeIterator() -> Iterator {
    return EnumIter(_base: _base.makeIterator())
  }
}

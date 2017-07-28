// RUN: %target-swift-frontend -enable-sil-opaque-values -enable-sil-ownership -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen -parse-stdlib -parse-as-library %s | %FileCheck %s

public protocol UnkeyedDecodingContainer {
  var isAtEnd: Builtin.Int1 { get }
}

public protocol Decoder {
  func unkeyedContainer() throws -> UnkeyedDecodingContainer
}

// Test open_existential_value ownership
// ---
// CHECK-LABEL: sil @_T016opaque_ownership11takeDecoderBi1_AA0D0_p4from_tKF : $@convention(thin) (@in Decoder) -> (Builtin.Int1, @error Builtin.NativeObject) {
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
// CHECK-LABEL: } // end sil function '_T016opaque_ownership11takeDecoderBi1_AA0D0_p4from_tKF'
public func takeDecoder(from decoder: Decoder) throws -> Builtin.Int1 {
  let container = try decoder.unkeyedContainer()
  return container.isAtEnd
}

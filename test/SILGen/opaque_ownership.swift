// RUN: %target-swift-frontend -enable-sil-opaque-values -enable-sil-ownership -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen -parse-stdlib -parse-as-library -module-name Swift %s | %FileCheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -enable-sil-opaque-values -enable-sil-ownership -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen -parse-stdlib -parse-as-library -module-name Swift %s | %FileCheck --check-prefix=CHECK-OSX %s

public typealias AnyObject = Builtin.AnyObject

precedencegroup AssignmentPrecedence {}
precedencegroup CastingPrecedence {}
precedencegroup ComparisonPrecedence {}

public protocol _ObjectiveCBridgeable {}

public protocol UnkeyedDecodingContainer {
  var isAtEnd: Builtin.Int1 { get }
}

public protocol Decoder {
  func unkeyedContainer() throws -> UnkeyedDecodingContainer
}

// Test open_existential_value ownership
// ---
// CHECK-LABEL: sil @_T0s11takeDecoderBi1_s0B0_p4from_tKF : $@convention(thin) (@in Decoder) -> (Builtin.Int1, @error Error) {
// CHECK: bb0(%0 : @owned $Decoder):
// CHECK:  [[BORROW1:%.*]] = begin_borrow %0 : $Decoder
// CHECK:  [[OPENED:%.*]] = open_existential_value [[BORROW1]] : $Decoder to $@opened("{{.*}}") Decoder
// CHECK:  [[WT:%.*]] = witness_method $@opened("{{.*}}") Decoder, #Decoder.unkeyedContainer!1 : <Self where Self : Decoder> (Self) -> () throws -> UnkeyedDecodingContainer, %4 : $@opened("{{.*}}") Decoder : $@convention(witness_method: Decoder) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out UnkeyedDecodingContainer, @error Error)
// CHECK:  try_apply [[WT]]<@opened("{{.*}}") Decoder>([[OPENED]]) : $@convention(witness_method: Decoder) <τ_0_0 where τ_0_0 : Decoder> (@in_guaranteed τ_0_0) -> (@out UnkeyedDecodingContainer, @error Error), normal bb2, error bb1
//
// CHECK:bb{{.*}}([[RET1:%.*]] : @owned $UnkeyedDecodingContainer):
// CHECK:  end_borrow [[BORROW1]] from %0 : $Decoder, $Decoder
// CHECK:  [[BORROW2:%.*]] = begin_borrow [[RET1]] : $UnkeyedDecodingContainer
// CHECK:  [[OPENED2:%.*]] = open_existential_value [[BORROW2]] : $UnkeyedDecodingContainer to $@opened("{{.*}}") UnkeyedDecodingContainer
// CHECK:  [[WT2:%.*]] = witness_method $@opened("{{.*}}") UnkeyedDecodingContainer, #UnkeyedDecodingContainer.isAtEnd!getter.1 : <Self where Self : UnkeyedDecodingContainer> (Self) -> () -> Builtin.Int1, [[OPENED2]] : $@opened("{{.*}}") UnkeyedDecodingContainer : $@convention(witness_method: UnkeyedDecodingContainer) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK:  [[RET2:%.*]] = apply [[WT2]]<@opened("{{.*}}") UnkeyedDecodingContainer>([[OPENED2]]) : $@convention(witness_method: UnkeyedDecodingContainer) <τ_0_0 where τ_0_0 : UnkeyedDecodingContainer> (@in_guaranteed τ_0_0) -> Builtin.Int1
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

// A lot of standard library support is necessary to support raw enums.
// --------------------------------------------------------------------

infix operator  == : ComparisonPrecedence
infix operator  ~= : ComparisonPrecedence

public struct Bool {
  var _value: Builtin.Int1

  public init() {
    let zero: Int64 = 0
    self._value = Builtin.trunc_Int64_Int1(zero._value)
  }

  internal init(_ v: Builtin.Int1) { self._value = v }

  public init(_ value: Bool) {
    self = value
  }
}

extension Bool {
  public func _getBuiltinLogicValue() -> Builtin.Int1 {
    return _value
  }
}

public protocol Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  ///
  /// Equality is the inverse of inequality. For any values `a` and `b`,
  /// `a == b` implies that `a != b` is `false`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  static func == (lhs: Self, rhs: Self) -> Bool
}

public func ~= <T : Equatable>(a: T, b: T) -> Bool {
  return a == b
}

public protocol RawRepresentable {
  associatedtype RawValue

  init?(rawValue: RawValue)

  var rawValue: RawValue { get }
}

public func == <T : RawRepresentable>(lhs: T, rhs: T) -> Bool
  where T.RawValue : Equatable {
  return lhs.rawValue == rhs.rawValue
}

public typealias _MaxBuiltinIntegerType = Builtin.Int2048

public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: _MaxBuiltinIntegerType)
}

public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral

  init(integerLiteral value: IntegerLiteralType)
}

extension ExpressibleByIntegerLiteral
  where Self : _ExpressibleByBuiltinIntegerLiteral {
  @_transparent
  public init(integerLiteral value: Self) {
    self = value
  }
}

public protocol ExpressibleByStringLiteral {}
public protocol ExpressibleByFloatLiteral {}
public protocol ExpressibleByUnicodeScalarLiteral {}
public protocol ExpressibleByExtendedGraphemeClusterLiteral {}

public struct Int64 : ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral, Equatable {
  public var _value: Builtin.Int64
  public init(_builtinIntegerLiteral x: _MaxBuiltinIntegerType) {
    _value = Builtin.s_to_s_checked_trunc_Int2048_Int64(x).0
  }
  public typealias IntegerLiteralType = Int64
  public init(integerLiteral value: Int64) {
    self = value
  }
  public static func ==(_ lhs: Int64, rhs: Int64) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }
}

// Test ownership of multi-case Enum values in the context of @trivial to @in thunks.
// ---
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @_T0s17FloatingPointSignOs9EquatablessACP2eeoiSbx_xtFZTW : $@convention(witness_method: Equatable) (@in FloatingPointSign, @in FloatingPointSign, @thick FloatingPointSign.Type) -> Bool {
// CHECK: bb0(%0 : @trivial $FloatingPointSign, %1 : @trivial $FloatingPointSign, %2 : @trivial $@thick FloatingPointSign.Type):
// CHECK:   %3 = function_ref @_T0s2eeoiSbx_xts16RawRepresentableRzs9Equatable0B5ValueRpzlF : $@convention(thin) <τ_0_0 where τ_0_0 : RawRepresentable, τ_0_0.RawValue : Equatable> (@in τ_0_0, @in τ_0_0) -> Bool
// CHECK:   %4 = apply %3<FloatingPointSign, Int64>(%0, %1) : $@convention(thin) <τ_0_0 where τ_0_0 : RawRepresentable, τ_0_0.RawValue : Equatable> (@in τ_0_0, @in τ_0_0) -> Bool
// CHECK:   return %4 : $Bool
// CHECK-LABEL: } // end sil function '_T0s17FloatingPointSignOs9EquatablessACP2eeoiSbx_xtFZTW'
public enum FloatingPointSign: Int64 {
  /// The sign for a positive value.
  case plus

  /// The sign for a negative value.
  case minus
}

#if os(OSX)
// Test open_existential_value used in a conversion context.
// (the actual bridging call is dropped because we don't import Swift).
// ---
// CHECK-OSX-LABEL: sil @_T0s26_unsafeDowncastToAnyObjectyXlyp04fromD0_tF : $@convention(thin) (@in Any) -> @owned AnyObject {
// CHECK-OSX: bb0(%0 : @owned $Any):
// CHECK-OSX:   [[BORROW:%.*]] = begin_borrow %0 : $Any
// CHECK-OSX:   [[COPY:%.*]] = copy_value [[BORROW]] : $Any
// CHECK-OSX:   [[BORROW2:%.*]] = begin_borrow [[COPY]] : $Any
// CHECK-OSX:   [[VAL:%.*]] = open_existential_value [[BORROW2]] : $Any to $@opened
// CHECK-OSX:   [[COPY2:%.*]] = copy_value [[VAL]] : $@opened
// CHECK-OSX:   destroy_value [[COPY2]] : $@opened
// CHECK-OSX:   end_borrow [[BORROW2]] from [[COPY]] : $Any, $Any
// CHECK-OSX:   destroy_value [[COPY]] : $Any
// CHECK-OSX:   end_borrow [[BORROW]] from %0 : $Any, $Any
// CHECK-OSX:   destroy_value %0 : $Any
// CHECK-OSX:   return undef : $AnyObject
// CHECK-OSX-LABEL: } // end sil function '_T0s26_unsafeDowncastToAnyObjectyXlyp04fromD0_tF'
public func _unsafeDowncastToAnyObject(fromAny any: Any) -> AnyObject {
  return any as AnyObject
}
#endif

public protocol Error {}

#if os(OSX)
// Test open_existential_box_value in a conversion context.
// ---
// CHECK-OSX-LABEL: sil @_T0s3fooys5Error_pSg1e_tF : $@convention(thin) (@owned Optional<Error>) -> () {
// CHECK-OSX: [[BORROW:%.*]] = begin_borrow %{{.*}} : $Error
// CHECK-OSX: [[VAL:%.*]] = open_existential_box_value [[BORROW]] : $Error to $@opened
// CHECK-OSX: [[COPY:%.*]] = copy_value [[VAL]] : $@opened
// CHECK-OSX: [[ANY:%.*]] = init_existential_value [[COPY]] : $@opened
// CHECK-OSX: end_borrow [[BORROW]] from %{{.*}} : $Error, $Error
// CHECK-OSX-LABEL: } // end sil function '_T0s3fooys5Error_pSg1e_tF'
public func foo(e: Error?) {
  if let u = e {
    let a: Any = u
    _ = a
  }
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
// CHECK:  [[MT:%.*]] = metatype $@thin EnumIter<Base.Iterator>.Type
// CHECK:  [[FIELD:%.*]] = struct_extract %0 : $EnumSeq<Base>, #EnumSeq._base
// CHECK:  [[COPY:%.*]] = copy_value [[FIELD]] : $Base
// CHECK:  [[BORROW:%.*]] = begin_borrow [[COPY]] : $Base
// CHECK:  [[WT:%.*]] = witness_method $Base, #Seq.makeIterator!1 : <Self where Self : Seq> (Self) -> () -> Self.Iterator : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  [[ITER:%.*]] = apply [[WT]]<Base>([[BORROW]]) : $@convention(witness_method: Seq) <τ_0_0 where τ_0_0 : Seq> (@in_guaranteed τ_0_0) -> @out τ_0_0.Iterator
// CHECK:  end_borrow [[BORROW]] from [[COPY]] : $Base, $Base
// CHECK:  destroy_value [[COPY]] : $Base
// CHECK: [[FN:%.*]] = function_ref @_T0s8EnumIterVAByxGx5_base_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : IP> (@in τ_0_0, @thin EnumIter<τ_0_0>.Type) -> @out EnumIter<τ_0_0>
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

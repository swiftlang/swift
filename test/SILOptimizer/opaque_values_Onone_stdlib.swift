// RUN: %target-swift-frontend -enable-experimental-move-only -parse-stdlib -module-name Swift -enable-sil-opaque-values -parse-as-library -Xllvm -sil-print-types -emit-sil -Onone %s | %FileCheck %s

// Like opaque_values_Onone.swift but for code that needs to be compiled with 
// -parse-stdlib.

protocol Error {}
enum Never : Error{}

precedencegroup AssignmentPrecedence { assignment: true }
precedencegroup CastingPrecedence {}

public protocol _ObjectiveCBridgeable {}
@_marker public protocol Copyable {}

public protocol _ExpressibleByBuiltinBooleanLiteral {
  init(_builtinBooleanLiteral value: Builtin.Int1)
}
struct Bool : _ExpressibleByBuiltinBooleanLiteral {
  var _value: Builtin.Int1
  @_silgen_name("Bool_init_noargs")
  init()
  init(_ v: Builtin.Int1) { self._value = v }
  init(_ value: Bool) { self = value }
  init(_builtinBooleanLiteral value: Builtin.Int1) {
    self._value = value
  }
}

@_silgen_name("typeof")
@_semantics("typechecker.type(of:)")
public func type<T, Metatype>(of value: T) -> Metatype

class X {}
func consume(_ x : __owned X) {}

func foo(@_noImplicitCopy _ x: __owned X) {
  consume(copy x)
  consume(x)
}

// CHECK-LABEL: sil hidden @getRawPointer : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[ADDR:%[^,]+]] : $*T):
// CHECK:         [[PTR:%[^,]+]] = address_to_pointer [stack_protection] [[ADDR]]
// CHECK:         return [[PTR]]
// CHECK-LABEL: } // end sil function 'getRawPointer'
@_silgen_name("getRawPointer")
func getRawPointer<T>(to value: T) -> Builtin.RawPointer {
  return Builtin.addressOfBorrow(value)
}

// CHECK-LABEL: sil hidden @getUnprotectedRawPointer : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[ADDR:%[^,]+]] : $*T):
// CHECK:         [[PTR:%[^,]+]] = address_to_pointer [[ADDR]]
// CHECK:         return [[PTR]]
// CHECK-LABEL: } // end sil function 'getUnprotectedRawPointer'
@_silgen_name("getUnprotectedRawPointer")
func getUnprotectedRawPointer<T>(to value: T) -> Builtin.RawPointer {
  return Builtin.unprotectedAddressOfBorrow(value)
}

// CHECK-LABEL: sil hidden @getBridgeObject : {{.*}} {
// CHECK:         [[OBJECT_ADDR:%[^,]+]] = unchecked_addr_cast {{%[^,]+}} : $*T to $*Builtin.BridgeObject
// CHECK:         [[OBJECT:%[^,]+]] = load [[OBJECT_ADDR]]
// CHECK:         return [[OBJECT]]
// CHECK-LABEL: } // end sil function 'getBridgeObject'
@_silgen_name("getBridgeObject")
func toObject<T>(_ object: inout T) -> Builtin.BridgeObject {
  Builtin.reinterpretCast(object)
}

// CHECK-LABEL: sil hidden @getAnotherType : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[RETADDR:%[^,]+]] : $*U, {{%[^,]+}} : $*T, {{%[^,]+}} : $@thick U.Type):
// CHECK:         [[OTHER_TY_ADDR:%[^,]+]] = unchecked_addr_cast {{%[^,]+}} : $*T to $*U
// CHECK:         copy_addr [[OTHER_TY_ADDR]] to [init] [[RETADDR]] : $*U
// CHECK-LABEL: } // end sil function 'getAnotherType'
@_silgen_name("getAnotherType")
func getAnotherType<T, U>(_ object: inout T, to ty: U.Type) -> U {
  Builtin.reinterpretCast(object)
}

@_silgen_name("isOfTypeOfAnyObjectType")
func isOfTypeOfAnyObjectType(fromAny any: Any) -> Bool {
  type(of: any) is Builtin.AnyObject.Type
}

@available(SwiftStdlib 5.1, *)
struct UnsafeContinuation<T, E: Error> {
  @usableFromInline internal var context: Builtin.RawUnsafeContinuation

// CHECK-LABEL: sil {{.*}}@unsafeContinuationResumeNoThrow : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[VALUE:%[^,]+]] : $*T, [[CONTINUATION:%[^,]+]] : $UnsafeContinuation<T, Never>):
// CHECK:         [[STACK:%[^,]+]] = alloc_stack $T
// CHECK:         [[CONTEXT:%[^,]+]] = struct_extract [[CONTINUATION]]
// CHECK:         copy_addr [[VALUE]] to [init] [[STACK]]
// CHECK:         builtin "resumeNonThrowingContinuationReturning"<T>([[CONTEXT]] : $Builtin.RawUnsafeContinuation, [[STACK]] : $*T)
// CHECK:         destroy_addr [[VALUE]]
// CHECK-LABEL: } // end sil function 'unsafeContinuationResumeNoThrow'
  @_silgen_name("unsafeContinuationResumeNoThrow")
  @_alwaysEmitIntoClient
  public func resume(returning value: __owned T) where E == Never {
    #if compiler(>=5.5) && $BuiltinContinuation
    Builtin.resumeNonThrowingContinuationReturning(context, value)
    #endif
  }

// CHECK-LABEL: sil {{.*}}@unsafeContinuationResumeThrow : {{.*}} {
// CHECK:       {{bb[0-9]+}}([[VALUE:%[^,]+]] : $*T, [[CONTINUATION:%[^,]+]] : $UnsafeContinuation<T, E>):
// CHECK:         [[STACK:%[^,]+]] = alloc_stack $T
// CHECK:         [[CONTEXT:%[^,]+]] = struct_extract [[CONTINUATION]]
// CHECK:         copy_addr [[VALUE]] to [init] [[STACK]]
// CHECK:         builtin "resumeThrowingContinuationReturning"<T>([[CONTEXT]] : $Builtin.RawUnsafeContinuation, [[STACK]] : $*T)
// CHECK:         destroy_addr [[VALUE]]
// CHECK-LABEL: } // end sil function 'unsafeContinuationResumeThrow'
  @_silgen_name("unsafeContinuationResumeThrow")
  @_alwaysEmitIntoClient
  public func resume(returning value: __owned T) {
    #if compiler(>=5.5) && $BuiltinContinuation
    Builtin.resumeThrowingContinuationReturning(context, value)
    #endif
  }
}

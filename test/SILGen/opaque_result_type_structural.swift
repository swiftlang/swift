// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-swift-5.1-abi-triple %s | %FileCheck %s

public protocol P {}

extension P {
  // This will only typecheck and SILGen if we emit the UnderlyingToOpaqueExpr
  // instead of a MetatypeConversionExpr
  public func f1() -> (some P).Type {
    return Self.self
  }

  // This will only typecheck and SILGen if we emit the UnderlyingToOpaqueExpr
  // instead of a FunctionConversionExpr
  public func f2() -> () -> (some P) {
    let fn = { self }
    return fn
  }

  // FIXME: This still doesn't work because the '() -> (some P)' propagates
  // backwards into the closure literal's return type
  /* public func f3() -> () -> (some P) {
    return { self }
  } */

  // Optionals already worked, but let's just be sure
  public func f4() -> (some P)? {
    return self
  }

  // Make sure we can handle a function conversion *and* opaque type erasure;
  // here we first convert (Any) -> (Self) to (Int) -> (Self) before erasing
  // to (Int) -> (some P)
  public func f5() -> (Int) -> (some P) {
    let fn: (Any) -> (Self) = { _ in self }
    return fn
  }
}

// Issues with metatypes
struct G<T> {}
struct S: P {}
extension G: P where T: P {}

func f2() -> G<some P>.Type {
  return G<S>.self
}

func g() -> Any {
  return f2()
}

// CHECK-LABEL: sil hidden [ossa] @$s29opaque_result_type_structural1gypyF : $@convention(thin) () -> @out Any {
// CHECK: bb0(%0 : $*Any):
// CHECK: [[METATYPE:%.*]] = metatype $@thick G<S>.Type
// CHECK: [[ADDR:%.*]] = init_existential_addr %0 : $*Any, $G<@_opaqueReturnTypeOf("$s29opaque_result_type_structural2f2AA1GVyQrGmyF", 0) __>.Type
// CHECK: store [[METATYPE]] to [trivial] [[ADDR]] : $*@thick G<S>.Type
// CHECK: return

// RUN: %target-swift-emit-silgen -disable-availability-checking %s

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


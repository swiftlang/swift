// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -emit-ir -sil-verify-all %s -o %t/opt.ll
// RUN: %target-swift-frontend -Onone -emit-ir -sil-verify-all %s -o %t/onone.ll

public struct Large {
  public var a: UInt64 = 0
  public var b: UInt64 = 0
  public var c: UInt64 = 0
  public var d: UInt64 = 0
  public var e: UInt64 = 0
  public var f: UInt64 = 0
  public var g: UInt64 = 0
  public var h: UInt64 = 0
}

@inline(never)
public func makeLarge(_ x: UInt64) -> Large {
  Large(a: x)
}

public var globalSum: UInt64 = 0

@inline(never)
public func sinkLarge(_ value: Large) {
  globalSum &+= value.a
}

public enum Callback {
  case none
  case thin(fn: @convention(thin) (UInt64) -> (), arg: UInt64)
}

public var savedCallback: Callback = .none

@inline(never)
public func saveCallback(_ cb: Callback) {
  savedCallback = cb
}

// Thin closure calls functions with large loadable parameters/results, so
// LoadableByAddress creates a local alloc_stack inside it, but the closure's
// own signature does not change and it is packed into a tuple enum payload.
public func registerCallback(_ x: UInt64) {
  let fn: @convention(thin) (UInt64) -> () = { a in
    sinkLarge(makeLarge(a))
  }
  fn(x)
  saveCallback(.thin(fn: fn, arg: x))
}

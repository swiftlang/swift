// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s
// REQUIRES: EnableSILOpaqueValues

func hasVarArg(_ args: Any...) {}

// ArgEmitter: fix SpecialDest args.
public func callVarArg() {
  hasVarArg(3)
}

public protocol P {
  var x : Int { get }
}

// SILGenFunction::emitOpenExistential: add an open_existential instruction.
public func foo(p: P) -> Int {
  return p.x
}

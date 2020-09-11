// RUN: %target-swift-emit-sil %s -verify
// REQUIRES: asserts

// TF-964: `PullbackEmitter::visitTupleInst` crash for `tuple` instructions with
// non-tuple-typed adjoint values.

@differentiable
func TF_964(_ x: Float) -> Float {
  let tuple = (x, 1)
  return tuple.0
}

// Original crasher:
// Assertion failed: (Operand->getType().is<TupleType>() && "Expected a tuple typed operand?!"), function create, file /Users/swiftninjas/s4tf/swift/lib/SIL/SILInstructions.cpp, line 2676.
// Stack dump:
// 0.	Program arguments: /Library/Developer/Toolchains/swift-tensorflow-RELEASE-0.6.xctoolchain/usr/bin/swift -frontend -interpret tf-964.swift -enable-objc-interop -sdk /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk -color-diagnostics -module-name main
// 1.	Swift version 5.1.1-dev (Swift 7b97b0ced0)
// 2.	While running pass #17 SILModuleTransform "Differentiation".
// 3.	While processing `[differentiable source 0 wrt 0]` attribute on SIL function "@$s4main6TF_964yS2fF".
//  for 'TF_964(_:)' (at tf-964.swift:2:1)
// 4.	While generating VJP for SIL function "@$s4main6TF_964yS2fF".
//  for 'TF_964(_:)' (at tf-964.swift:2:1)
// 5.	While generating pullback for SIL function "@$s4main6TF_964yS2fF".
//  for 'TF_964(_:)' (at tf-964.swift:2:1)

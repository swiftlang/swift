// RUN: not %target-swift-frontend -c -primary-file %s %S/Inputs/tf953-invalid-differentiable-attr-other-module.swift -module-name main

// Verify that invalid `@differentiable` attribute in non-primary file does not
// crash SILGen.

func bar(_ x: Int) -> Float {
  return foo(2)
}

// Assertion failed: (paramIndices && "Parameter indices should have been resolved"), function addFunctionAttributes, file /Users/swiftninjas/s4tf/swift/lib/SIL/SILFunctionBuilder.cpp, line 97.
// Stack dump:
// 1.	Swift version 5.1.1-dev (Swift e242a8825f)
// 2.	While emitting SIL for 'bar(_:)' (at /Users/danielzheng/swift-merge/swift/test/AutoDiff/compiler_crashers/tf953-invalid-differentiable-attr-cross-module.swift:3:1)
// 3.	While silgen emitFunction SIL function "@$s4main3barySfSiF".
//  for 'bar(_:)' (at /Users/danielzheng/swift-merge/swift/test/AutoDiff/compiler_crashers/tf953-invalid-differentiable-attr-cross-module.swift:3:1)

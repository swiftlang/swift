// RUN: %target-swift-frontend -c %s -g -O -parse-stdlib -parse-as-library -module-name Swift

// TF-597: Exact minimal reproducer for IRGenDebugInfo crash.
// Crash occurred only with `-g` and `-O`.
//
// ```
// Assertion failed: (OffsetInBits + SizeInBits <= getSizeInBits(Var) && "pars > totum"),
// function emitVariableDeclaration, file swift/lib/IRGen/IRGenDebugInfo.cpp, line 2216.
// Stack dump:
// 1.	Swift version 5.1-dev (LLVM 200186e28b, Swift c09c14dec5)
// 2.	While emitting IR SIL function "@$ss8pullback2at2inyx_q_xXFts14DifferentiableRzsADR_r0_lF".
//  for 'pullback(at:in:)' (at swift/test/AutoDiff/differentiable_func_debuginfo.swift:21:8)
// ```
//
// The crash was because `IRGenDebugInfoImpl::getOrCreateType` computes
// `llvm::DIType` type debug info by demangling type names.
//
// Since `@differentiable` and `@differentiable(linear)` function types did
// not have mangling support, `getOrCreateType` computed a regular `(A) -> B`
// function type instead of a `@differentiable (A) -> B` function type, leading
// to a type size inconsistency.
//
// Conclusion: mangling coverage is important.

// Minimal dummy compiler-known `Differentiable` protocol.
public protocol Differentiable {
  associatedtype TangentVector
}

// This declaration is necessary to reproduce the crash for some reason.
public func blackHole<T, U>(_: (T) -> U) {}

public func pullback<T, R>(
  at x: T, in tf597ProblematicVarDecl: @differentiable (T) -> R
) {
  let _ = Builtin.autodiffApply_vjp(tf597ProblematicVarDecl, x)
}

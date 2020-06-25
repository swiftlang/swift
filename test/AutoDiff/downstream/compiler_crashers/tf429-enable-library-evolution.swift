// RUN: not --crash %target-swift-emit-sil -enable-library-evolution %s
// REQUIRES: asserts

// TF-429: Differentiation transform does not support
// `-enable-library-evolution` because it assumes that differential/pullback
// structs are always loadable, i.e. have object value category.

// Function must be public to trigger library evolution crash.
@differentiable
public func TF_429(_ x: Float) -> Float { x }

// Assertion failed: (mainPullbackStruct->getType() == pbStructLoweredType), function run, file /Users/danielzheng/swift-merge/swift/lib/SILOptimizer/Mandatory/Differentiation.cpp, line 6279.
// Stack dump:
// ...
// 1.	Swift version 5.1.1-dev (Swift c3cdcba346)
// 2.	While running pass #17 SILModuleTransform "Differentiation".
// ...
// 7  swiftc                   0x0000000101620642 (anonymous namespace)::PullbackEmitter::run() + 3122
// 8  swiftc                   0x00000001015cb1e8 (anonymous namespace)::VJPEmitter::run() + 1224
// 9  swiftc                   0x00000001015c3348 (anonymous namespace)::ADContext::processDifferentiableAttribute(swift::SILFunction*, swift::SILDifferentiableAttr*, (anonymous namespace)::DifferentiationInvoker) + 4536

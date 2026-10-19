// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// REQUIRES: objc_interop

// The `AnyObject` cases of noncopyable_is_relaxed_targets.swift, which depend on
// `__SwiftValue` boxing and so need Objective-C interop. The rest of those
// cases run on every platform.

protocol P: ~Copyable {}

struct OnlyP: P, ~Copyable { var t = 1 }
struct Copy: P { var t = 3 }
final class Ref: P { var t = 4 }

// MARK: - A. existential source, AnyObject target

func isAnyObject(_ x: consuming any P & ~Copyable) -> Bool { x is AnyObject }

// `AnyObject` takes a class directly and boxes other Copyable values.
// CHECK: A OnlyP is AnyObject: false
print("A OnlyP is AnyObject:", isAnyObject(OnlyP() as any P & ~Copyable))
// CHECK: A Copy  is AnyObject: true
print("A Copy  is AnyObject:", isAnyObject(Copy() as any P & ~Copyable))
// CHECK: A Ref   is AnyObject: true
print("A Ref   is AnyObject:", isAnyObject(Ref() as any P & ~Copyable))

// MARK: - B. concrete noncopyable source, AnyObject target

func concreteIsAnyObject(_ x: consuming OnlyP) -> Bool { x is AnyObject }

// CHECK: B OnlyP is AnyObject: false
print("B OnlyP is AnyObject:", concreteIsAnyObject(OnlyP()))

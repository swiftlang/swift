// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.0-abi-triple %s -module-name test | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-PRESENT %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.10-abi-triple %s -module-name test | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-SUPPRESSED %s

// REQUIRES: OS=macosx
// UNSUPPORTED: CPU=arm64e

// A same-type generic requirement whose right-hand side is a type that needs
// the Swift 6.0 runtime demangler (here, a nominal type with an inverse
// requirement) and that is still type-parameter dependent used to crash IRGen
// when deploying to a pre-6.0 runtime: the dangling-metadata-accessor fallback
// was built without the enclosing generic signature, so the dependent type
// could not be mapped into a generic environment.

public struct Inner<T: ~Copyable> {}

public protocol P { associatedtype A }

// CHECK: @"$s4test5CrashVMn" =
// The second type of the `U.A == Inner<T>` requirement is emitted as a relative
// reference to a mangled type-ref. On a 6.0+ runtime that is a plain symbolic
// mangled name; on an older runtime it is a dangling accessor function (which
// must capture the generic signature so the dependent `Inner<T>` can be bound).
// CHECK-PRESENT: @"symbolic{{.*}}4test5InnerVAARi_zrlE"
// CHECK-SUPPRESSED: @"get_type_metadata{{.*}}5InnerVyxG{{.*}}"
public struct Crash<T, U: P> where U.A == Inner<T> {}

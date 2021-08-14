// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -typecheck -F %S/Inputs/requirement-conflict -module-cache-path %t/clang-module-cache %s

// REQUIRES: objc_interop
// N.B. technically only need objc_interop here, but CoreFeatures would have to
// be cross-compiled for more architectures. It's sufficient to verify this
// works for macOS alone since we only care that we don't emit errors.
// REQUIRES: OS=macosx

import CoreFeatures

// The RootObject class in CoreFeatures is intentionally quite strange.
// Formally, RootObject declares a conformance to an Objective-C protocol called
// 'ReactiveRootProtocol', but in Swift instead of ObjC. When we go to import
// the ObjC side of the framework, we notice this and mirror-in the requirements
// from its ancestor 'RootProtocol'. With that accomplished, we now have an ObjC
// class imported through Swift with ObjC members mirrored on top. This combo
// used to be toxic - we would see the very members we synthesized during the
// shadowing pass and would diagnose them as directly conflicting with the
// protocol conformance we had declared in Swift. That is - we would say
// a requirement conflicted directly... with itself!
//
// Nowadays we just exempt interface files from this kind of checking, which is
// the same behavior one would get if they had set up this very scenario with
// a plain swiftmodule file.
let root = CoreFeatures.RootObject()
print(root)

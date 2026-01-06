// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck --check-prefix=CHECK-WITHOUT %s 
// RUN: %target-typecheck-verify-swift -enable-experimental-static-assert
// RUN: %target-typecheck-verify-swift -enable-experimental-feature StaticAssert

// REQUIRES: swift_feature_StaticAssert

#if compiler(>=5.3) && $StaticAssert
#assert(true)
#else
// CHECK-WITHOUT: cannot find 'complete' in scope
complete junk
#endif

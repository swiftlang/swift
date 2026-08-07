// RUN: %target-swift-frontend -O -wmo -module-name=test -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

// Verify the performance inliner folds the body of an @objc method into
// its native-to-foreign thunk and DCE's the original body, when the body
// has no indirect dispatch path (final / static / private / not-
// overridden) and the thunk is its only direct caller. See
// PerformanceInliner.cpp's isProfitableToInline thunk path.

import Foundation

// final + internal + above the trivial inline threshold + sole direct
// caller is the @objc thunk. Body must be inlined and the original DCE'd.
final class FinalBridged: NSObject {
  @objc func compute(_ x: Int) -> Int {
    var y = x &* 3
    y = y &+ 7
    y = y &* x
    y = y &+ 11
    y = y &* x
    y = y &+ 13
    y = y &* x
    y = y &+ 17
    y = y &* x
    return y
  }
}

// CHECK-LABEL: sil {{.*}}[thunk] {{.*}}@$s4test12FinalBridgedC7computeyS2iFTo
// CHECK-NOT:     function_ref @$s4test12FinalBridgedC7computeyS2iF :
// CHECK:       } // end sil function '$s4test12FinalBridgedC7computeyS2iFTo'

// CHECK-NOT: sil {{.*}}@$s4test12FinalBridgedC7computeyS2iF :

// Non-final class with a `static` @objc method. `static` methods are
// statically dispatched (no vtable slot), so the same wash applies and
// the body should be inlined into the thunk.
class StaticBridged: NSObject {
  @objc static func compute(_ x: Int) -> Int {
    var y = x &* 3
    y = y &+ 7
    y = y &* x
    y = y &+ 11
    y = y &* x
    y = y &+ 13
    y = y &* x
    y = y &+ 17
    y = y &* x
    return y
  }
}

// CHECK-LABEL: sil {{.*}}[thunk] {{.*}}@$s4test13StaticBridgedC7computeyS2iFZTo
// CHECK-NOT:     function_ref @$s4test13StaticBridgedC7computeyS2iFZ :
// CHECK:       } // end sil function '$s4test13StaticBridgedC7computeyS2iFZTo'

// CHECK-NOT: sil {{.*}}@$s4test13StaticBridgedC7computeyS2iFZ :

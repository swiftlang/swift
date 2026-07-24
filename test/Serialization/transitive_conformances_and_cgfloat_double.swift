// RUN: %target-swift-frontend -emit-module %s -o %t -solver-disable-transitive-conformance
// RUN: %target-typecheck-verify-swift -solver-enable-transitive-conformance

// FIXME: Removing -solver-enable-transitive-conformance and the associated
// optimization will fix the salvage issue.

// REQUIRES: objc_interop

// rdar://153461854

import CoreGraphics

@resultBuilder
public struct Builder {
  public static func buildBlock<T1>(_ t1: T1) -> (T1) {
    return (t1)
  }
}

protocol P {
}

struct Proxy: P {
  let to: (CGFloat?) -> Void
}

struct Test {
  @Builder var proxy: some P {  // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    Proxy { point in
      if let point {
        let _: SIMD2<Double>? = SIMD2(point, point)
      }
    }
  }
}


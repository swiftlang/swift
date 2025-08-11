// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module %s -o %t

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
  @Builder var proxy: some P {
    Proxy { point in
      if let point {
        let _: SIMD2<Double>? = SIMD2(point, point)
      }
    }
  }
}


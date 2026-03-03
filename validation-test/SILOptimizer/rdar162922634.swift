// RUN: %target-swift-frontend -O -emit-sil -verify %s

// REQUIRES: objc_interop

import Foundation

func f(_ x: [AnyHashable]) -> [NSObject.Type]? {
  return x as? [NSObject.Type]
}

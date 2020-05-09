// RUN: %target-typecheck-verify-swift %clang-importer-sdk
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -verify %s
// REQUIRES: objc_interop

import Foundation

func f(_: [AnyObject]) {}
func g(_: [Protocol]) {}

f([NSString.self, NSObject.self])

@objc protocol P: AnyObject {}
@objc protocol Q: AnyObject {}

func foo(p: P.Type, pq: (P & Q).Type) {
  f([p, pq])
}

g([P.self, Q.self])

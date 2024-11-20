// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_RegionBasedIsolation

import Foundation

class X: NSObject {
  // expected-error@+1 {{'async' initializer cannot be represented in Objective-C}}
  @objc init(_ i : Int) async { }
}


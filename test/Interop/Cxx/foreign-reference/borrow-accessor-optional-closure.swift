// RUN: %target-swift-frontend -emit-sil -O -wmo -parse-as-library -target %target-swift-5.8-abi-triple -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -I %S/Inputs %s -verify -o /dev/null

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_LifetimeDependence

// Inlining a borrow accessor into a closure that switches over the optional
// result used to leave the borrow open along the `none` path, which crashed
// the ownership verifier in CopyPropagation.

import BorrowAccessorOptionalClosure

struct Storage: ~Copyable {
  var object: Object? = nil
}

extension Owner {
  var object: Object? {
    @_unsafeSelfDependentResult
    borrow { unsafe raw!.assumingMemoryBound(to: Storage.self).pointee.object }
  }
}

public func read(_ owner: Owner) -> Int32 {
  return {
    if let object = owner.object {
      return object.value()
    }
    return 0
  }()
}

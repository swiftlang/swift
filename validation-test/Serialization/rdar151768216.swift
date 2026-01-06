// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -o %t -enable-experimental-feature LifetimeDependence

// RUN: %target-swift-frontend -emit-sil %t/rdar151768216.swiftmodule \
// RUN: -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

// Ensure no crash
extension Result {   
  @inlinable
  func castError(i: Int, j: Int, k: Int) -> Result<Success, Failure> {
    return self.mapError { error in
      return error
    }   
  }
}


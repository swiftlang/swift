// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

// This should pass without any warnings or errors

@MainActor
var floofer: Int = 42

@main struct Doggo { }

extension Doggo {
  static func main() {
    print("Doggo value: \(floofer)")
  }
}

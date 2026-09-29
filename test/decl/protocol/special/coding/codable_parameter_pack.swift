// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// We should accept this:

public struct HasPack<each T>: Codable {
  var x: String?
}

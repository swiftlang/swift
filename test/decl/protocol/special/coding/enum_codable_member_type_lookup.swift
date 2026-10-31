// RUN: %target-typecheck-verify-swift -verify-ignore-unknown
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// MARK: - Synthesized CodingKeys Enum

// Enums which get synthesized Codable implementations should have visible
// CodingKey enums during member type lookup.
enum SynthesizedEnum : Codable {
  case value

}


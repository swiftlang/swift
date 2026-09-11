// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum EnumWithExcludedElement : Codable {
    case x
    case y

    enum CodingKeys: CodingKey {
        case x
    }
}

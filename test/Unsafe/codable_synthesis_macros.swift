// RUN: %target-typecheck-verify-swift -strict-memory-safety -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)
// RUN: %target-typecheck-verify-swift -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

@unsafe public struct UnsafeStruct: Codable {
  public var string: String
}

@unsafe public enum UnsafeEnum: Codable {
case something(Int)
}

@safe public struct SafeStruct: Codable {
  public var us: UnsafeStruct
}

@safe public enum SafeEnum: Codable {
case something(UnsafeEnum)
}

public struct PlainStruct: Codable {
  public var string: String
}

public enum PlainEnum: Codable {
case something(Int)
}

// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -parse-as-library -wmo -O %s -o /dev/null
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -parse-as-library -wmo -Osize %s -o /dev/null

// REQUIRES: swift_feature_Embedded

// Projecting a key path with a zero-sized root and a zero-sized enum value
// (no cases or one case) used to crash IRGen.

public enum Empty {}
public enum One { case a }

public struct R {
  public let x: One
}

public struct Q {
  public var x: One { .a }
}

public struct E {
  public var e: Empty { fatalError() }
}

public func f() -> AnyKeyPath { \R.x }
public func g() -> AnyKeyPath { \Q.x }
public func h() -> AnyKeyPath { \E.e }

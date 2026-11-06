// RUN: %target-typecheck-verify-swift -parse-as-library -enable-library-evolution

// REQUIRES: OS=macosx

// Back-deploying a struct or enum requires BackDeployFrozenValueTypes.

@backDeployed(before: macOS 99) // expected-error {{'@backDeployed' is an experimental feature; use '-enable-experimental-feature BackDeployFrozenValueTypes'}}
@frozen
public struct S {
  public var x: Int
}

@backDeployed(before: macOS 99) // expected-error {{'@backDeployed' is an experimental feature; use '-enable-experimental-feature BackDeployFrozenValueTypes'}}
@frozen
public enum E {
  case a, b
}

// Functions don't need the feature.
@backDeployed(before: macOS 99)
public func f() {}

// RUN: %target-typecheck-verify-swift -enable-experimental-feature TargetAttribute -disable-availability-checking

// REQUIRES: swift_feature_TargetAttribute

@_target("") // expected-error {{'@_target' requires a non-empty target string}}
func emptyString(_ x: Int) -> Int { return x }

// @_target cannot be combined with @_transparent
@_target("default") // expected-error {{'@_target' cannot be used with '@_transparent'}}
@_transparent
func incompatibleWithTransparent(_ x: Int) -> Int { return x }

@_target("tune=generic,tune=generic") // expected-error {{duplicate 'tune=' option in '@_target'}}
func duplicateTune(_ x: Int) -> Int { return x }

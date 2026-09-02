// RUN: %target-typecheck-verify-swift -enable-experimental-feature TargetAttribute -disable-availability-checking -target arm64-apple-macosx13.0

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=arm64

@_target("sve2")
func validFeature(_ x: Int) -> Int { return x }

@_target("cpu=apple-m1")
func validCPU(_ x: Int) -> Int { return x }

@_target("cpu=apple-m1+dotprod")
func validCPUWithSuffix(_ x: Int) -> Int { return x }

@_target("cpu=bogus-cpu-name") // expected-error {{unsupported CPU 'bogus-cpu-name' in '@_target'}}
func invalidCPU(_ x: Int) -> Int { return x }

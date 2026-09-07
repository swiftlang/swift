// RUN: %target-typecheck-verify-swift -enable-experimental-feature TargetAttribute -disable-availability-checking -target x86_64-apple-macosx13.0

// REQUIRES: swift_feature_TargetAttribute
// REQUIRES: OS=macosx && CPU=x86_64

@_target("avx2")
func validFeature(_ x: Int) -> Int { return x }

@_target("no-sse4.2")
func validNegatedFeature(_ x: Int) -> Int { return x }

@_target("arch=skylake")
func validArch(_ x: Int) -> Int { return x }

@_target("tune=icelake-client")
func validTune(_ x: Int) -> Int { return x }

@_target("sve2") // expected-error {{unsupported target feature 'sve2' in '@_target'}}
func invalidFeature(_ x: Int) -> Int { return x }

@_target("arch=bogus-cpu-name") // expected-error {{unsupported CPU 'bogus-cpu-name' in '@_target'}}
func invalidCPU(_ x: Int) -> Int { return x }

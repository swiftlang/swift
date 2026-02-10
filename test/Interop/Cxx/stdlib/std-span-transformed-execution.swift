// RUN: %target-run-simple-swift(-plugin-path %swift-plugin-dir -I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -swift-version 6 -Xfrontend -disable-availability-checking -Xcc -std=c++20 -enable-experimental-feature LifetimeDependence -enable-experimental-feature StabilizedSafeInteropWrappers)

// TODO: test failed in Windows PR testing: rdar://144384453
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: swift_feature_StabilizedSafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// REQUIRES: executable_test
// REQUIRES: std_span

#if !BRIDGING_HEADER
import StdSpan
#endif
import CxxStdlib

func canCallSafeSpanAPIs(_ x: Span<CInt>) {
    funcWithSafeWrapper(x)
    funcWithSafeWrapper2(x)
}

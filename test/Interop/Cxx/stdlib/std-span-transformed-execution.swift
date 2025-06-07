// RUN: %target-run-simple-swift(-plugin-path %swift-plugin-dir -I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -swift-version 6 -Xfrontend -disable-availability-checking -Xcc -std=c++20 -enable-experimental-feature LifetimeDependence -enable-experimental-feature SafeInteropWrappers)

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

// TODO: test failed in Windows PR testing: rdar://144384453
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// REQUIRES: executable_test

#if !BRIDGING_HEADER
import StdSpan
#endif
import CxxStdlib

func canCallSafeSpanAPIs(_ x: Span<CInt>) {
    funcWithSafeWrapper(x)
    funcWithSafeWrapper2(x)
}

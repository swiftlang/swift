// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded %target-embedded-posix-shim -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h -wmo)

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_embedded_platform

func test() {
  var major = 0
  var minor = 0
  swift_getPlatformLayerVersion(&major, &minor)
  
  precondition(major == EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR)
  precondition(minor == EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR)
}

test()

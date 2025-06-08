// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

@available(OSX 10.16, *)
func introducedOnMacOS10_16() { }

@available(OSX 11.0, *)
func introducedOnMacOS11_0() { }

@available(macOS 16.0, iOS 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *)
func introducedInVersionsMappingTo26_0() { }

@available(macOS 26.0, iOS 26.0, watchOS 26.0, tvOS 26.0, visionOS 26.0, *)
func introducedIn26_0() { }

func useUnderPoundAvailable() {
  if #available(OSX 10.16, *) {
    introducedOnMacOS10_16()
    introducedOnMacOS11_0()
  }

  if #available(macOS 16.0, iOS 19.0, watchOS 12.0, tvOS 19.0, visionOS 3.0, *) {
    introducedInVersionsMappingTo26_0()
    introducedIn26_0()
  }
}

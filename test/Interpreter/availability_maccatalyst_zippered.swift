// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/macos_module)
// RUN: %empty-directory(%t/maccatalyst_module)


// Build a zippered library

// Emit a zippered library and a module for use from macOSProcesses
// RUN: %target-build-swift-dylib(%t/%target-library-name(availability_zippered)) %S/Inputs/availability_zippered.swift -target %target-cpu-apple-macosx10.15 -target-variant %target-cpu-apple-ios13.1-macabi -emit-module -emit-module-path %t/macos_module/
// RUN: %target-codesign %t/%target-library-name(availability_zippered)

// Emit just an macCatalyst module for use from an macCatalyst process
// RUN: %target-build-swift %S/Inputs/availability_zippered.swift -target %target-cpu-apple-ios13.1-macabi -emit-module -emit-module-path %t/maccatalyst_module/

// Build a macOS executable that calls into the library
// RUN: %target-build-swift -emit-executable -target %target-cpu-apple-macosx10.15 %s -lavailability_zippered -I %t/macos_module -L %t -o %t/a-macos.out
// RUN: %target-codesign %t/a-macos.out

// Built an macCatalyst executable that calls into the library.
// RUN: %target-build-swift -target %target-cpu-apple-ios13.1-macabi %s -lavailability_zippered -I %t/maccatalyst_module -L %t -o %t/a-maccatalyst.out
// RUN: %target-codesign %t/a-maccatalyst.out

// RUN: %target-run %t/a-macos.out %t/%target-library-name(availability_zippered)
// RUN: %target-run %t/a-maccatalyst.out %t/%target-library-name(availability_zippered)

// REQUIRES: executable_test
// REQUIRES: maccatalyst_support
// REQUIRES: OS=maccatalyst || OS=macosx


import availability_zippered

// The purpose of this test is to check the semantics of #available in zippered
// code. Executables themselves can't be zippered (only libraries) so we
// call into a zippered dylib.

import StdlibUnittest

var ZipperedAvailabilityTestSuite = TestSuite("ZipperedAvailabilityTestSuite")

#if os(macOS)
ZipperedAvailabilityTestSuite.test("zippered availability macOS") {
  expectFalse(isMacOSAfterFarFutureOriOSAfterFarFuture())
  expectTrue(isMacOSAfterDistantPastOriOSAfterDistantPast())

  expectTrue(isMacOSAfterDistantPastOriOSAfterFarFuture())
  expectFalse(isMacOSAfterFarFutureOriOSAfterDistantPast())

  expectTrue(isMacOSAfterDistantPastOriOSAfterFarFuture())
  expectFalse(isMacOSAfterFarFutureOriOSAfterDistantPast())
}
#endif

#if os(iOS)
ZipperedAvailabilityTestSuite.test("zippered availability iOS") {
  expectFalse(isMacOSAfterFarFutureOriOSAfterFarFuture())
  expectTrue(isMacOSAfterDistantPastOriOSAfterDistantPast())

  // These tests will fail on systems earlier than macOS 10.15.
  expectFalse(isMacOSAfterDistantPastOriOSAfterFarFuture())
  expectTrue(isMacOSAfterFarFutureOriOSAfterDistantPast())
  expectFalse(isMacOSAfterDistantPastOriOSAfterFarFuture())
  expectTrue(isMacOSAfterFarFutureOriOSAfterDistantPast())
}
#endif


runAllTests()

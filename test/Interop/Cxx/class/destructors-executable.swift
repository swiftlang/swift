// RUN: %target-run-simple-swift(-g -I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import DebugInfo

@available(macOS 13.3, *)
func test(d: CppReceiver, s: Passed) {
  d.callMe(s)
}

// func someFunc() {
//   _ = SomeType(value: 1)
// }

// someFunc()

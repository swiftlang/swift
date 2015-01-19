// RUN: %target-parse-verify-swift
// REQUIRES: OS=ios

import Foundation

func test_unavailable_because_deprecated() {
  println(NSRealMemoryAvailable()) // expected-error {{APIs deprecated as of iOS 7 and earlier are unavailable in Swift}}
}


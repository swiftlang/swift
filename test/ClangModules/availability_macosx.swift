// RUN: %target-parse-verify-swift

// REQUIRES: OS=macosx

import Foundation

func test_unavailable_because_deprecated() {
  println(NSRealMemoryAvailable()) // expected-error {{APIs deprecated as of OS X 10.9 and earlier are unavailable in Swift}}
}


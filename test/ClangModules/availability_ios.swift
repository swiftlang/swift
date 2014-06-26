// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target -target arm64-apple-ios7.0 %s
// REQUIRES: CPU=arm64, OS=ios

import Foundation

func test_unavailable_because_deprecated() {
  println(NSRealMemoryAvailable()) // expected-error {{APIs deprecated as of iOS 7 and earlier are unavailable in Swift}}
}


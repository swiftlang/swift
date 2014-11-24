// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -application-extension %s
// XFAIL: linux

import Foundation

func test_unavailable_app_extension() {
  println(SomeCrazyAppExtensionForbiddenAPI()) // expected-error {{'SomeCrazyAppExtensionForbiddenAPI()' is unavailable: Not available in App Extensions}}
}

// RUN: %swift %clang-importer-sdk -parse -verify -target x86_64-apple-macosx10.9 -application-extension %s

import Foundation

func test_unavailable_app_extension() {
  println(SomeCrazyAppExtensionForbiddenAPI()) // expected-error {{'SomeCrazyAppExtensionForbiddenAPI()' is unavailable: Not available in App Extensions}}
}

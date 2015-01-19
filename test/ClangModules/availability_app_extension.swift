// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify -application-extension %s

import Foundation

func test_unavailable_app_extension() {
  println(SomeCrazyAppExtensionForbiddenAPI()) // expected-error {{'SomeCrazyAppExtensionForbiddenAPI()' is unavailable: Not available in App Extensions}}
}

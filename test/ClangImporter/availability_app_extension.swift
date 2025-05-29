// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -application-extension %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -application-extension-library %s

// Check the exact error message, which requires a regex match
// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -application-extension %s 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func test_unavailable_app_extension() {
  _ = SomeCrazyAppExtensionForbiddenAPI() // expected-error {{unavailable}}
// CHECK: error: 'SomeCrazyAppExtensionForbiddenAPI()' is unavailable in application extensions for {{[a-z]+}}OS: Not available in App Extensions
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-error-handling -emit-silgen -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation
import exceptions

func test0() {
  try ErrorProne.fail() // expected-error {{errors thrown from here are not handled}}
}
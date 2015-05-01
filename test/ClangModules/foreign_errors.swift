// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation
import errors

func test0() {
  try ErrorProne.fail() // expected-error {{errors thrown from here are not handled}}
}

// Test "AndReturnError" stripping.
// rdar://20722195
func testAndReturnError() throws {
  try ErrorProne.fail()
  try ErrorProne.go()
  try ErrorProne.tryAndReturnError() // collides with 'try' keyword
}

func testInheritedInit() throws {
  try ReallyErrorProne(one: nil)
}

func testInheritedFactory() throws {
  try ReallyErrorProne(two: nil)
}

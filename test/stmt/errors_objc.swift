// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

// Catching `as NSError` ought to be exhaustive when ObjC interop is enabled.
func bar() throws {}

func foo() {
  do {
    try bar()
  } catch _ as NSError {
  }
}

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// REQUIRES: objc_interop

import Foundation

func useAnyObject(_ obj: AnyObject) {
  _ = obj.isMakingHoney
}

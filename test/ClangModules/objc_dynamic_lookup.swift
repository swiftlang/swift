// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -enable-swift-name-lookup-tables %s -verify

// REQUIRES: objc_interop

import Foundation

func useAnyObject(obj: AnyObject) {
  _ = obj.makingHoney
}
